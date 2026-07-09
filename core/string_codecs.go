package core

import (
	"fmt"
	"strconv"
	"strings"
)

// normalizeEncodingName lowercases a Python codec name and strips the '-', '_'
// and space separators, so aliases such as "Latin-1", "latin_1" and "LATIN1"
// all collapse to the same key.
func normalizeEncodingName(name string) string {
	var b strings.Builder
	for _, r := range strings.ToLower(name) {
		if r == '-' || r == '_' || r == ' ' {
			continue
		}
		b.WriteRune(r)
	}
	return b.String()
}

// EncodeString encodes s with one of the text codecs M28 supports natively
// (utf-8, latin-1, ascii and their common aliases). ok is false when the
// encoding is not one of these, so callers keep their existing
// unknown-encoding error path. A latin-1/ascii range violation is returned as
// a ValueError, matching CPython's UnicodeEncodeError message shape.
func EncodeString(s, encoding string) (result []byte, ok bool, err error) {
	switch normalizeEncodingName(encoding) {
	case "utf8", "u8", "utf", "cp65001":
		return []byte(s), true, nil
	case "latin1", "iso88591", "8859", "cp819", "latin", "l1":
		out := make([]byte, 0, len(s))
		pos := 0
		for _, r := range s {
			if r > 0xFF {
				return nil, true, &ValueError{Message: fmt.Sprintf(
					"'latin-1' codec can't encode character '\\u%04x' in position %d: ordinal not in range(256)", r, pos)}
			}
			out = append(out, byte(r))
			pos++
		}
		return out, true, nil
	case "ascii", "usascii", "646", "us":
		out := make([]byte, 0, len(s))
		pos := 0
		for _, r := range s {
			if r > 0x7F {
				return nil, true, &ValueError{Message: fmt.Sprintf(
					"'ascii' codec can't encode character '\\u%04x' in position %d: ordinal not in range(128)", r, pos)}
			}
			out = append(out, byte(r))
			pos++
		}
		return out, true, nil
	case "rawunicodeescape":
		// Code points 0-255 map to their byte; higher ones become \uXXXX (BMP)
		// or \UXXXXXXXX escapes.
		var out []byte
		for _, r := range s {
			switch {
			case r < 0x100:
				out = append(out, byte(r))
			case r <= 0xFFFF:
				out = append(out, fmt.Sprintf("\\u%04x", r)...)
			default:
				out = append(out, fmt.Sprintf("\\U%08x", r)...)
			}
		}
		return out, true, nil
	}
	return nil, false, nil
}

// DecodeBytes decodes b with one of the text codecs M28 supports natively. ok
// is false for unknown encodings. An ascii range violation is returned as a
// ValueError, matching CPython's UnicodeDecodeError message shape.
func DecodeBytes(b []byte, encoding string) (result string, ok bool, err error) {
	switch normalizeEncodingName(encoding) {
	case "utf8", "u8", "utf", "cp65001":
		return string(b), true, nil
	case "latin1", "iso88591", "8859", "cp819", "latin", "l1":
		var sb strings.Builder
		sb.Grow(len(b))
		for _, c := range b {
			sb.WriteRune(rune(c))
		}
		return sb.String(), true, nil
	case "ascii", "usascii", "646", "us":
		var sb strings.Builder
		sb.Grow(len(b))
		for i, c := range b {
			if c > 0x7F {
				return "", true, &ValueError{Message: fmt.Sprintf(
					"'ascii' codec can't decode byte 0x%02x in position %d: ordinal not in range(128)", c, i)}
			}
			sb.WriteByte(c)
		}
		return sb.String(), true, nil
	case "rawunicodeescape":
		// Each byte is code point 0-255; \uXXXX and \UXXXXXXXX escapes decode to
		// the corresponding code point.
		var sb strings.Builder
		i := 0
		for i < len(b) {
			if b[i] == '\\' && i+1 < len(b) && (b[i+1] == 'u' || b[i+1] == 'U') {
				width := 4
				if b[i+1] == 'U' {
					width = 8
				}
				if i+2+width <= len(b) {
					if cp, perr := strconv.ParseUint(string(b[i+2:i+2+width]), 16, 32); perr == nil {
						sb.WriteRune(rune(cp))
						i += 2 + width
						continue
					}
				}
			}
			sb.WriteRune(rune(b[i]))
			i++
		}
		return sb.String(), true, nil
	}
	return "", false, nil
}
