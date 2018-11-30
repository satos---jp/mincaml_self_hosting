rule main = parse
| "a" { 4 }
| "cb" { 5 }
| "bc" { 6 }
| "abc" { 1 }
| "acb" { 2 }
| ("ac" | "bc") "cab" { 3 }
| "end" { -1 }

