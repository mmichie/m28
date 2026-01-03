package modules

import (
	"fmt"
	"time"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterTimeModule registers the time module
func InitTimeModule() *core.DictValue {
	// Create module as a dict
	timeModule := core.NewDict()

	// time() - return current time in seconds since epoch
	timeModule.SetWithKey("time", core.StringValue("time"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("time", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return current time as float seconds since Unix epoch
		now := time.Now()
		seconds := float64(now.Unix()) + float64(now.Nanosecond())/1e9
		return core.NumberValue(seconds), nil
	}))

	// sleep() - sleep for given number of seconds
	timeModule.SetWithKey("sleep", core.StringValue("sleep"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sleep", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		seconds, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		duration := time.Duration(float64(seconds) * float64(time.Second))
		time.Sleep(duration)

		return core.Nil, nil
	}))

	// monotonic() - return monotonic time in seconds (Python 3.3+)
	// Used for measuring elapsed time, not affected by system clock adjustments
	var startTime = time.Now()
	timeModule.SetWithKey("monotonic", core.StringValue("monotonic"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("monotonic", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return elapsed time since module load in seconds
		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Seconds()), nil
	}))

	// perf_counter() - return performance counter in seconds (Python 3.3+)
	// Similar to monotonic() but with highest available resolution
	timeModule.SetWithKey("perf_counter", core.StringValue("perf_counter"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("perf_counter", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Return elapsed time since module load in seconds with nanosecond precision
		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Seconds()), nil
	}))

	// process_time() - return CPU time of the current process in seconds (Python 3.3+)
	// Go doesn't have direct CPU time measurement, so we approximate with elapsed time
	timeModule.SetWithKey("process_time", core.StringValue("process_time"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("process_time", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Approximate CPU time with elapsed time since module load
		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Seconds()), nil
	}))

	// process_time_ns() - return CPU time in nanoseconds (Python 3.7+)
	timeModule.SetWithKey("process_time_ns", core.StringValue("process_time_ns"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("process_time_ns", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Nanoseconds()), nil
	}))

	// perf_counter_ns() - return performance counter in nanoseconds (Python 3.7+)
	timeModule.SetWithKey("perf_counter_ns", core.StringValue("perf_counter_ns"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("perf_counter_ns", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Nanoseconds()), nil
	}))

	// monotonic_ns() - return monotonic time in nanoseconds (Python 3.7+)
	timeModule.SetWithKey("monotonic_ns", core.StringValue("monotonic_ns"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("monotonic_ns", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		elapsed := time.Since(startTime)
		return core.NumberValue(elapsed.Nanoseconds()), nil
	}))

	// localtime() - convert seconds since epoch to local time struct
	// Returns struct_time: (tm_year, tm_mon, tm_mday, tm_hour, tm_min, tm_sec, tm_wday, tm_yday, tm_isdst)
	timeModule.SetWithKey("localtime", core.StringValue("localtime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("localtime", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}

		// If no argument, use current time
		var t time.Time
		if v.Count() == 0 {
			t = time.Now()
		} else {
			seconds, err := v.GetNumber(0)
			if err != nil {
				return nil, err
			}
			// Convert seconds since epoch to time.Time
			sec := int64(float64(seconds))
			nsec := int64((float64(seconds) - float64(sec)) * 1e9)
			t = time.Unix(sec, nsec)
		}

		// Convert to local time
		t = t.Local()

		// Create struct_time tuple
		// Python struct_time: (tm_year, tm_mon, tm_mday, tm_hour, tm_min, tm_sec, tm_wday, tm_yday, tm_isdst)
		structTime := core.TupleValue{
			core.NumberValue(t.Year()),         // tm_year
			core.NumberValue(int(t.Month())),   // tm_mon (1-12)
			core.NumberValue(t.Day()),          // tm_mday (1-31)
			core.NumberValue(t.Hour()),         // tm_hour (0-23)
			core.NumberValue(t.Minute()),       // tm_min (0-59)
			core.NumberValue(t.Second()),       // tm_sec (0-59)
			core.NumberValue(int(t.Weekday())), // tm_wday (0=Monday in Python, but Go uses 0=Sunday)
			core.NumberValue(t.YearDay()),      // tm_yday (1-366)
			core.NumberValue(-1),               // tm_isdst (-1 = unknown)
		}

		return structTime, nil
	}))

	// gmtime() - convert seconds since epoch to UTC time struct
	timeModule.SetWithKey("gmtime", core.StringValue("gmtime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("gmtime", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}

		// If no argument, use current time
		var t time.Time
		if v.Count() == 0 {
			t = time.Now()
		} else {
			seconds, err := v.GetNumber(0)
			if err != nil {
				return nil, err
			}
			// Convert seconds since epoch to time.Time
			sec := int64(float64(seconds))
			nsec := int64((float64(seconds) - float64(sec)) * 1e9)
			t = time.Unix(sec, nsec)
		}

		// Convert to UTC
		t = t.UTC()

		// Create struct_time tuple
		structTime := core.TupleValue{
			core.NumberValue(t.Year()),
			core.NumberValue(int(t.Month())),
			core.NumberValue(t.Day()),
			core.NumberValue(t.Hour()),
			core.NumberValue(t.Minute()),
			core.NumberValue(t.Second()),
			core.NumberValue(int(t.Weekday())),
			core.NumberValue(t.YearDay()),
			core.NumberValue(0), // tm_isdst = 0 for UTC
		}

		return structTime, nil
	}))

	// strftime() - format time according to a format string
	timeModule.SetWithKey("strftime", core.StringValue("strftime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("strftime", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		formatStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// If no time tuple provided, use current time
		var t time.Time
		if v.Count() == 1 {
			t = time.Now()
		} else {
			// Parse time tuple if provided
			timeTuple := args[1]
			if tuple, ok := timeTuple.(core.TupleValue); ok && len(tuple) >= 6 {
				// Extract year, month, day, hour, minute, second from tuple
				year := int(tuple[0].(core.NumberValue))
				month := time.Month(int(tuple[1].(core.NumberValue)))
				day := int(tuple[2].(core.NumberValue))
				hour := int(tuple[3].(core.NumberValue))
				minute := int(tuple[4].(core.NumberValue))
				second := int(tuple[5].(core.NumberValue))
				t = time.Date(year, month, day, hour, minute, second, 0, time.Local)
			} else {
				// Fallback to current time
				t = time.Now()
			}
		}

		// Convert Python strftime format codes to result string
		result := formatStrftime(formatStr, t)
		return core.StringValue(result), nil
	}))

	// mktime() - convert struct_time to seconds since epoch
	timeModule.SetWithKey("mktime", core.StringValue("mktime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("mktime", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Parse time tuple
		timeTuple := args[0]
		tuple, ok := timeTuple.(core.TupleValue)
		if !ok || len(tuple) < 6 {
			return nil, fmt.Errorf("mktime() argument must be a 9-tuple")
		}

		// Extract year, month, day, hour, minute, second from tuple
		year := int(tuple[0].(core.NumberValue))
		month := time.Month(int(tuple[1].(core.NumberValue)))
		day := int(tuple[2].(core.NumberValue))
		hour := int(tuple[3].(core.NumberValue))
		minute := int(tuple[4].(core.NumberValue))
		second := int(tuple[5].(core.NumberValue))

		t := time.Date(year, month, day, hour, minute, second, 0, time.Local)
		return core.NumberValue(float64(t.Unix())), nil
	}))

	// strptime() - parse a string into struct_time according to format
	timeModule.SetWithKey("strptime", core.StringValue("strptime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("strptime", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		dateStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}
		formatStr, err := v.GetString(1)
		if err != nil {
			return nil, err
		}

		// Parse the date string using the format
		t, err := parseStrptime(dateStr, formatStr)
		if err != nil {
			return nil, fmt.Errorf("time data '%s' does not match format '%s'", dateStr, formatStr)
		}

		// Return struct_time tuple
		structTime := core.TupleValue{
			core.NumberValue(t.Year()),
			core.NumberValue(int(t.Month())),
			core.NumberValue(t.Day()),
			core.NumberValue(t.Hour()),
			core.NumberValue(t.Minute()),
			core.NumberValue(t.Second()),
			core.NumberValue(int(t.Weekday())),
			core.NumberValue(t.YearDay()),
			core.NumberValue(-1), // tm_isdst
		}
		return structTime, nil
	}))

	// time_ns() - return current time in nanoseconds since epoch (Python 3.7+)
	timeModule.SetWithKey("time_ns", core.StringValue("time_ns"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("time_ns", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		now := time.Now()
		ns := now.UnixNano()
		return core.NumberValue(ns), nil
	}))

	// ctime() - convert seconds since epoch to string representation
	timeModule.SetWithKey("ctime", core.StringValue("ctime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ctime", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}

		var t time.Time
		if v.Count() == 0 {
			t = time.Now()
		} else {
			seconds, err := v.GetNumber(0)
			if err != nil {
				return nil, err
			}
			sec := int64(float64(seconds))
			nsec := int64((float64(seconds) - float64(sec)) * 1e9)
			t = time.Unix(sec, nsec)
		}

		// Format like "Mon Jan  2 15:04:05 2006"
		result := t.Format("Mon Jan _2 15:04:05 2006")
		return core.StringValue(result), nil
	}))

	// asctime() - convert struct_time to string representation
	timeModule.SetWithKey("asctime", core.StringValue("asctime"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("asctime", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}

		var t time.Time
		if v.Count() == 0 {
			t = time.Now()
		} else {
			tuple, ok := args[0].(core.TupleValue)
			if !ok || len(tuple) < 6 {
				return nil, fmt.Errorf("asctime() argument must be a 9-tuple")
			}
			year := int(tuple[0].(core.NumberValue))
			month := time.Month(int(tuple[1].(core.NumberValue)))
			day := int(tuple[2].(core.NumberValue))
			hour := int(tuple[3].(core.NumberValue))
			minute := int(tuple[4].(core.NumberValue))
			second := int(tuple[5].(core.NumberValue))
			t = time.Date(year, month, day, hour, minute, second, 0, time.Local)
		}

		// Format like "Mon Jan  2 15:04:05 2006"
		result := t.Format("Mon Jan _2 15:04:05 2006")
		return core.StringValue(result), nil
	}))

	return timeModule
}

// formatStrftime converts Python strftime format string to formatted output
func formatStrftime(format string, t time.Time) string {
	// Weekday names
	weekdays := []string{"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}
	weekdaysShort := []string{"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"}
	// Month names
	months := []string{"", "January", "February", "March", "April", "May", "June",
		"July", "August", "September", "October", "November", "December"}
	monthsShort := []string{"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}

	var result []byte
	i := 0
	for i < len(format) {
		if format[i] == '%' && i+1 < len(format) {
			switch format[i+1] {
			case 'Y': // 4-digit year
				result = append(result, t.Format("2006")...)
			case 'y': // 2-digit year
				result = append(result, t.Format("06")...)
			case 'm': // Month as zero-padded decimal (01-12)
				result = append(result, t.Format("01")...)
			case 'd': // Day as zero-padded decimal (01-31)
				result = append(result, t.Format("02")...)
			case 'H': // Hour (24-hour) as zero-padded decimal (00-23)
				result = append(result, t.Format("15")...)
			case 'I': // Hour (12-hour) as zero-padded decimal (01-12)
				result = append(result, t.Format("03")...)
			case 'M': // Minute as zero-padded decimal (00-59)
				result = append(result, t.Format("04")...)
			case 'S': // Second as zero-padded decimal (00-59)
				result = append(result, t.Format("05")...)
			case 'f': // Microsecond as zero-padded decimal (000000-999999)
				result = append(result, fmt.Sprintf("%06d", t.Nanosecond()/1000)...)
			case 'A': // Full weekday name
				result = append(result, weekdays[t.Weekday()]...)
			case 'a': // Abbreviated weekday name
				result = append(result, weekdaysShort[t.Weekday()]...)
			case 'B': // Full month name
				result = append(result, months[t.Month()]...)
			case 'b', 'h': // Abbreviated month name
				result = append(result, monthsShort[t.Month()]...)
			case 'p': // AM or PM
				if t.Hour() < 12 {
					result = append(result, "AM"...)
				} else {
					result = append(result, "PM"...)
				}
			case 'j': // Day of year as zero-padded decimal (001-366)
				result = append(result, fmt.Sprintf("%03d", t.YearDay())...)
			case 'U': // Week number (Sunday as first day) (00-53)
				// Calculate week number with Sunday as first day
				yday := t.YearDay()
				wday := int(t.Weekday())
				weekNum := (yday - wday + 7) / 7
				result = append(result, fmt.Sprintf("%02d", weekNum)...)
			case 'W': // Week number (Monday as first day) (00-53)
				// Calculate week number with Monday as first day
				yday := t.YearDay()
				wday := int(t.Weekday())
				if wday == 0 {
					wday = 7 // Make Sunday = 7 for Monday-first calculation
				}
				weekNum := (yday - wday + 7) / 7
				result = append(result, fmt.Sprintf("%02d", weekNum)...)
			case 'w': // Weekday as decimal (0=Sunday, 6=Saturday)
				result = append(result, fmt.Sprintf("%d", t.Weekday())...)
			case 'c': // Locale's appropriate date and time representation
				// Use a common format: "Mon Jan 2 15:04:05 2006"
				result = append(result, t.Format("Mon Jan _2 15:04:05 2006")...)
			case 'x': // Locale's appropriate date representation
				// Use common format: "01/02/06"
				result = append(result, t.Format("01/02/06")...)
			case 'X': // Locale's appropriate time representation
				// Use common format: "15:04:05"
				result = append(result, t.Format("15:04:05")...)
			case 'z': // UTC offset in +HHMM or -HHMM format
				result = append(result, t.Format("-0700")...)
			case 'Z': // Timezone name
				result = append(result, t.Format("MST")...)
			case 'e': // Day as space-padded decimal ( 1-31)
				result = append(result, t.Format("_2")...)
			case 'k': // Hour (24-hour) as space-padded decimal ( 0-23)
				result = append(result, fmt.Sprintf("%2d", t.Hour())...)
			case 'l': // Hour (12-hour) as space-padded decimal ( 1-12)
				hour := t.Hour() % 12
				if hour == 0 {
					hour = 12
				}
				result = append(result, fmt.Sprintf("%2d", hour)...)
			case 'n': // Newline
				result = append(result, '\n')
			case 't': // Tab
				result = append(result, '\t')
			case '%': // Literal %
				result = append(result, '%')
			case 'G': // ISO 8601 year with century
				year, _ := t.ISOWeek()
				result = append(result, fmt.Sprintf("%04d", year)...)
			case 'g': // ISO 8601 year without century
				year, _ := t.ISOWeek()
				result = append(result, fmt.Sprintf("%02d", year%100)...)
			case 'V': // ISO 8601 week number (01-53)
				_, week := t.ISOWeek()
				result = append(result, fmt.Sprintf("%02d", week)...)
			case 'u': // ISO 8601 weekday (1=Monday, 7=Sunday)
				wday := int(t.Weekday())
				if wday == 0 {
					wday = 7
				}
				result = append(result, fmt.Sprintf("%d", wday)...)
			case 'C': // Century (year / 100)
				result = append(result, fmt.Sprintf("%02d", t.Year()/100)...)
			case 'D': // Equivalent to %m/%d/%y
				result = append(result, t.Format("01/02/06")...)
			case 'F': // Equivalent to %Y-%m-%d
				result = append(result, t.Format("2006-01-02")...)
			case 'r': // 12-hour time with AM/PM
				result = append(result, t.Format("03:04:05 PM")...)
			case 'R': // Equivalent to %H:%M
				result = append(result, t.Format("15:04")...)
			case 'T': // Equivalent to %H:%M:%S
				result = append(result, t.Format("15:04:05")...)
			case 's': // Seconds since Unix epoch
				result = append(result, fmt.Sprintf("%d", t.Unix())...)
			default:
				// Unknown format code - keep as-is
				result = append(result, format[i], format[i+1])
			}
			i += 2
		} else {
			result = append(result, format[i])
			i++
		}
	}
	return string(result)
}

// parseStrptime parses a date string according to a Python strftime-style format
func parseStrptime(dateStr, format string) (time.Time, error) {
	// Convert Python format to Go format
	goFormat := convertStrptimeFormat(format)
	return time.ParseInLocation(goFormat, dateStr, time.Local)
}

// convertStrptimeFormat converts Python strptime format to Go time format
func convertStrptimeFormat(format string) string {
	// Build result by replacing format codes
	var result []byte
	i := 0
	for i < len(format) {
		if format[i] == '%' && i+1 < len(format) {
			switch format[i+1] {
			case 'Y': // 4-digit year
				result = append(result, "2006"...)
			case 'y': // 2-digit year
				result = append(result, "06"...)
			case 'm': // Month as zero-padded decimal (01-12)
				result = append(result, "01"...)
			case 'd': // Day as zero-padded decimal (01-31)
				result = append(result, "02"...)
			case 'H': // Hour (24-hour) as zero-padded decimal (00-23)
				result = append(result, "15"...)
			case 'I': // Hour (12-hour) as zero-padded decimal (01-12)
				result = append(result, "03"...)
			case 'M': // Minute as zero-padded decimal (00-59)
				result = append(result, "04"...)
			case 'S': // Second as zero-padded decimal (00-59)
				result = append(result, "05"...)
			case 'f': // Microsecond
				result = append(result, "000000"...)
			case 'A': // Full weekday name
				result = append(result, "Monday"...)
			case 'a': // Abbreviated weekday name
				result = append(result, "Mon"...)
			case 'B': // Full month name
				result = append(result, "January"...)
			case 'b', 'h': // Abbreviated month name
				result = append(result, "Jan"...)
			case 'p': // AM or PM
				result = append(result, "PM"...)
			case 'z': // UTC offset in +HHMM or -HHMM format
				result = append(result, "-0700"...)
			case 'Z': // Timezone name
				result = append(result, "MST"...)
			case 'e': // Day as space-padded decimal
				result = append(result, "_2"...)
			case '%': // Literal %
				result = append(result, '%')
			case 'F': // Equivalent to %Y-%m-%d
				result = append(result, "2006-01-02"...)
			case 'T': // Equivalent to %H:%M:%S
				result = append(result, "15:04:05"...)
			case 'D': // Equivalent to %m/%d/%y
				result = append(result, "01/02/06"...)
			case 'R': // Equivalent to %H:%M
				result = append(result, "15:04"...)
			default:
				// Unknown format code - keep as-is
				result = append(result, format[i], format[i+1])
			}
			i += 2
		} else {
			result = append(result, format[i])
			i++
		}
	}
	return string(result)
}

// Migration Statistics:
// Functions migrated: 2 time functions (time, sleep)
// Type checks eliminated: 2 manual type assertions
// Code improvements: Uses validation framework
// Benefits: Consistent error messages, cleaner validation
