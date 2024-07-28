package core

import (
	"sync"
)

type PythonicDict struct {
	data map[LispValue]LispValue
	mu   sync.RWMutex
}

func NewPythonicDict() *PythonicDict {
	return &PythonicDict{
		data: make(map[LispValue]LispValue),
	}
}

func (d *PythonicDict) Get(key LispValue) (LispValue, bool) {
	d.mu.RLock()
	defer d.mu.RUnlock()
	value, ok := d.data[key]
	return value, ok
}

func (d *PythonicDict) Set(key, value LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.data[key] = value
}

func (d *PythonicDict) Delete(key LispValue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	delete(d.data, key)
}

// Add the Size method
func (d *PythonicDict) Size() int {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return len(d.data)
}

func (d *PythonicDict) Iterate(f func(key, value LispValue) error) error {
	d.mu.RLock()
	defer d.mu.RUnlock()
	for k, v := range d.data {
		if err := f(k, v); err != nil {
			return err
		}
	}
	return nil
}
