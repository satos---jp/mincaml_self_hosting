rule main = parse
| "abc" { 1 }
| "acb" { 2 }
| ("ac" | "bc") "cab" { 3 }
| "end" { -1 }

