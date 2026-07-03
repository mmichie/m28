package core

import (
	"fmt"
	"strings"
)

// This file implements C3 linearization — the real Python MRO. The cached
// linearization is the single source of truth for method/attribute resolution:
// Class.GetMethod, GetMethodWithClass, GetClassAttr, __mro__/mro(), super()
// and hash's findUserHash all walk it, so ordinary lookup and cooperative
// super() can never disagree about resolution order.
//
// Class hierarchies are immutable after creation in M28 (__bases__ cannot be
// reassigned), so the cache computed on first use never goes stale.

// Linearization returns the C3 MRO of c, starting with c itself and ending at
// object. The result is cached on the class. An inconsistent hierarchy returns
// a TypeError matching CPython's message.
func (c *Class) Linearization() ([]*Class, error) {
	if c.mro != nil || c.mroErr != nil {
		return c.mro, c.mroErr
	}
	mro, err := c3Linearize(c, make(map[*Class]bool))
	if err != nil {
		c.mroErr = err
		return nil, err
	}
	c.mro = mro
	return mro, nil
}

// c3Linearize computes L(c) = c + merge(L(P1), ..., L(Pn), [P1, ..., Pn]).
// The in-progress set guards against parent cycles (impossible via normal
// class statements, cheap to be safe against hand-built hierarchies).
func c3Linearize(c *Class, inProgress map[*Class]bool) ([]*Class, error) {
	if inProgress[c] {
		return nil, &TypeError{Message: fmt.Sprintf("a __bases__ cycle exists involving class '%s'", c.Name)}
	}
	if len(c.Parents) == 0 {
		return []*Class{c}, nil
	}
	inProgress[c] = true
	defer delete(inProgress, c)

	seqs := make([][]*Class, 0, len(c.Parents)+1)
	for _, p := range c.Parents {
		if p == nil {
			continue
		}
		pl, err := p.Linearization()
		if err != nil {
			return nil, err
		}
		// merge consumes its sequences; copy so cached slices stay intact.
		seqs = append(seqs, append([]*Class(nil), pl...))
	}
	parents := make([]*Class, 0, len(c.Parents))
	for _, p := range c.Parents {
		if p != nil {
			parents = append(parents, p)
		}
	}
	seqs = append(seqs, parents)

	merged, ok := c3Merge(seqs)
	if !ok {
		names := make([]string, 0, len(parents))
		for _, p := range parents {
			names = append(names, p.Name)
		}
		return nil, &TypeError{Message: fmt.Sprintf(
			"Cannot create a consistent method resolution order (MRO) for bases %s",
			strings.Join(names, ", "))}
	}
	return append([]*Class{c}, merged...), nil
}

// c3Merge repeatedly takes the first head that appears in no sequence's tail.
func c3Merge(seqs [][]*Class) ([]*Class, bool) {
	var result []*Class
	for {
		// Drop exhausted sequences.
		live := seqs[:0]
		for _, s := range seqs {
			if len(s) > 0 {
				live = append(live, s)
			}
		}
		seqs = live
		if len(seqs) == 0 {
			return result, true
		}

		var winner *Class
		for _, s := range seqs {
			head := s[0]
			inTail := false
			for _, t := range seqs {
				for _, cls := range t[1:] {
					if cls == head {
						inTail = true
						break
					}
				}
				if inTail {
					break
				}
			}
			if !inTail {
				winner = head
				break
			}
		}
		if winner == nil {
			return nil, false // no valid head: inconsistent hierarchy
		}

		result = append(result, winner)
		for i, s := range seqs {
			if len(s) > 0 && s[0] == winner {
				seqs[i] = s[1:]
			}
		}
	}
}

// mroOrParentsWalk returns the cached C3 order, or nil if the hierarchy is
// inconsistent (callers then fall back to the legacy parent walk — such
// classes can only exist if creation-time validation was bypassed).
func (c *Class) mroOrNil() []*Class {
	mro, err := c.Linearization()
	if err != nil {
		return nil
	}
	return mro
}

// InvalidateMRO drops the cached linearization for c. Only needed if a
// hierarchy is ever mutated after creation (no current code path does).
func (c *Class) InvalidateMRO() {
	c.mro = nil
	c.mroErr = nil
}

// slotsRestrict reports whether instances of c reject attribute creation
// outside __slots__ — true only when every class in the MRO (other than
// object) declares __slots__ and none of them includes "__dict__". Cached on
// the class; class hierarchies are immutable after creation.
func (c *Class) slotsRestrict() bool {
	if c.slotsRestricted != nil {
		return *c.slotsRestricted
	}
	restricted := c.SlotNames != nil
	if restricted {
		mro := c.mroOrNil()
		if mro == nil {
			restricted = false
		} else {
		scan:
			for _, k := range mro {
				if k.Name == "object" {
					continue
				}
				if k.SlotNames == nil {
					restricted = false
					break
				}
				for _, s := range k.SlotNames {
					if s == "__dict__" {
						restricted = false
						break scan
					}
				}
			}
		}
	}
	c.slotsRestricted = &restricted
	return restricted
}
