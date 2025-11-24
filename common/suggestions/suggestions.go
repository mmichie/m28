package suggestions

import (
	"sort"
	"strings"
)

// Candidate represents a suggestion candidate with its distance score
type Candidate struct {
	Name     string
	Distance int
}

// FindSimilarNames finds similar names to the target within maxDistance edit distance
// Returns up to maxResults suggestions, sorted by similarity (closest first)
func FindSimilarNames(target string, candidates []string, maxDistance, maxResults int) []string {
	if len(candidates) == 0 {
		return nil
	}

	// Calculate distances for all candidates
	scored := make([]Candidate, 0, len(candidates))
	for _, candidate := range candidates {
		distance := LevenshteinDistance(target, candidate)
		if distance <= maxDistance {
			scored = append(scored, Candidate{
				Name:     candidate,
				Distance: distance,
			})
		}
	}

	// No matches found
	if len(scored) == 0 {
		return nil
	}

	// Sort by distance (closest first), then alphabetically for ties
	sort.Slice(scored, func(i, j int) bool {
		if scored[i].Distance != scored[j].Distance {
			return scored[i].Distance < scored[j].Distance
		}
		return scored[i].Name < scored[j].Name
	})

	// Return up to maxResults
	results := make([]string, 0, maxResults)
	for i := 0; i < len(scored) && i < maxResults; i++ {
		results = append(results, scored[i].Name)
	}

	return results
}

// FindSimilarNamesCaseInsensitive is like FindSimilarNames but ignores case
func FindSimilarNamesCaseInsensitive(target string, candidates []string, maxDistance, maxResults int) []string {
	lowerTarget := strings.ToLower(target)

	// Create case-insensitive candidates map
	candidateMap := make(map[string]string) // lowercase -> original
	for _, c := range candidates {
		candidateMap[strings.ToLower(c)] = c
	}

	// Get unique lowercase candidates
	lowerCandidates := make([]string, 0, len(candidateMap))
	for lc := range candidateMap {
		lowerCandidates = append(lowerCandidates, lc)
	}

	// Find similar lowercase names
	lowerResults := FindSimilarNames(lowerTarget, lowerCandidates, maxDistance, maxResults)

	// Map back to original case
	results := make([]string, 0, len(lowerResults))
	for _, lr := range lowerResults {
		if orig, ok := candidateMap[lr]; ok {
			results = append(results, orig)
		}
	}

	return results
}

// FormatSuggestion formats a suggestion message
func FormatSuggestion(suggestions []string) string {
	if len(suggestions) == 0 {
		return ""
	}

	if len(suggestions) == 1 {
		return "Did you mean '" + suggestions[0] + "'?"
	}

	// Multiple suggestions
	quoted := make([]string, len(suggestions))
	for i, s := range suggestions {
		quoted[i] = "'" + s + "'"
	}

	if len(suggestions) == 2 {
		return "Did you mean " + quoted[0] + " or " + quoted[1] + "?"
	}

	// 3 or more suggestions
	var sb strings.Builder
	sb.WriteString("Did you mean ")
	for i := 0; i < len(quoted)-1; i++ {
		sb.WriteString(quoted[i])
		sb.WriteString(", ")
	}
	sb.WriteString("or ")
	sb.WriteString(quoted[len(quoted)-1])
	sb.WriteString("?")
	return sb.String()
}
