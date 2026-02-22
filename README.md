# Gaul

**A programming language where identifiers can have spaces, keywords speak your language, and approximate equality is a first-class citizen.**

```gaul
fn Calculate Portfolio Risk(Initial Investment, Risk Factor) {
    let My Very Special Adjustment = 0.95
    Initial Investment * Risk Factor * My Very Special Adjustment
}

let My Portfolio Value = Calculate Portfolio Risk(1_000_000, 0.85)
println(My Portfolio Value)
```

Yes, `My Very Special Adjustment` is a valid variable name. Yes, `Calculate Portfolio Risk` is a valid function name. Welcome to Gaul.

---

## What is Gaul?

Gaul (from Indonesian *"bahasa gaul"* — slang language) is a dynamically-typed, interpreted programming language built in Rust. It's designed to challenge conventions: What if variable names could read like natural language? What if you could program in your native tongue? What if "close enough" was a real operator?

### Key Features

- **Spaces in identifiers** — Write `let My Variable = 42` instead of `myVariable` or `my_variable`
- **Custom keywords** — Program in French, Mandarin, Indonesian, or any language you define
- **Modules** — Split code across files with `import` / `export`
- **Functional programming** — First-class functions, closures, map/filter/reduce
- **The Jam Karet operator** — Fuzzy equality (`~=`) for numbers and strings
- **Full Unicode support** — Variables, strings, and comments in any script
- **Expression-based** — Everything returns a value

---

## Quick Start

```bash
# Clone and build
git clone https://github.com/yourusername/gaul-lang
cd gaul-lang
cargo build --release

# Run a script
./target/release/gaul-lang script.gaul

# Or start the REPL
./target/release/gaul-lang
```

---

## Language Tour

### Variables with Personality

```gaul
let My First Variable = 100
var Mutable Counter = 0

let My Very Risky Risk Factor = 0.95
let User Input Validation Result = true
```

Identifiers terminate at operators, punctuation, or newlines — so the parser knows where your multi-word name ends.

### Functions That Read Like English

```gaul
fn Calculate Total Price(Base Price, Tax Rate, Discount Percentage) {
    let After Discount = Base Price * (1 - Discount Percentage)
    After Discount * (1 + Tax Rate)
}

let Final Price = Calculate Total Price(100, 0.08, 0.15)
println(Final Price)  // 91.8
```

### Control Flow

```gaul
// If/else are expressions - they return values
let Result = if(Score >= 90) {
    "A"
} else {
    if(Score >= 80) { "B" } else { "C" }
}

// Loops
while(Counter < 10) {
    Counter = Counter + 1
}

for(Item : My Array) {
    println(Item)
}

// Ranges
for(I : 1..10) {
    println(I)
}
```

### Operators

Gaul has the usual arithmetic (`+`, `-`, `*`, `/`), comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`), and logical (`&&`, `||`, `!`) operators, compound assignment (`+=`, `-=`, `*=`, `/=`), plus bitwise operators for integer-level manipulation:

```gaul
5 & 3      // 1 (AND)
5 | 3      // 7 (OR)
5 ^ 3      // 6 (XOR)
1 << 3     // 8 (left shift)
8 >> 2     // 2 (right shift)
~5         // -6 (bitwise NOT)
```

Modulo: `10 % 3` — uses Euclidean semantics (result is always non-negative).

### Functional Programming

Gaul supports first-class functions, closures, and the holy trinity of functional programming:

```gaul
let Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Map: transform each element
let Doubled = Numbers.map(fn(X) { X * 2 })
// [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

// Filter: keep elements matching predicate
let Evens = Numbers.filter(fn(X) { X % 2 == 0 })
// [2, 4, 6, 8, 10]

// Reduce: accumulate to single value
let Sum = Numbers.reduce(0, fn(Acc, X) { Acc + X })
// 55

// Chain them together
let Result = Numbers
    .filter(fn(X) { X % 2 == 0 })
    .map(fn(X) { X * X })
    .sum()
// 220
```

### Maps

Maps are key-value dictionaries. Create them with `[key: value]` syntax, using `[:]` for an empty map. Keys can be strings, numbers, booleans, or null.

```gaul
// Create a map
let Person = ["name": "Alice", "age": 30, "active": true]

// Access values
Person.get("name")             // "Alice"
Person.get("missing")          // null (no error)
Person.get_or("job", "none")   // "none" (default for missing key)

// Modify
Person.set("email", "alice@example.com")
Person.remove("email")         // returns the removed value

// Check
Person.has("name")             // true
Person.len()                   // 3
Person.is_empty()              // false

// Iterate over keys
for(Key : Person) {
    println(Key)
}

// Get all keys, values, or entries
Person.keys()                  // ["name", "age", "active"]
Person.values()                // ["Alice", 30, true]
Person.entries()               // [["name", "Alice"], ["age", 30], ...]
```

Maps are perfect for frequency counting, memoization, and lookup tables:

```gaul
let Words = ["apple", "banana", "apple", "cherry", "banana", "apple"]
let Counts = [:]

for(Word : Words) {
    Counts.set(Word, Counts.get_or(Word, 0) + 1)
}

// Counts = ["apple": 3, "banana": 2, "cherry": 1]
```

### Closures

```gaul
fn makeCounter() {
    var count = 0
    fn increment() {
        count = count + 1
        println(count)
    }
    return increment
}

let counter = makeCounter()
counter()  // 1
counter()  // 2
counter()  // 3
```

---

## The Jam Karet Operator (`~=`)

*"Jam karet"* is Indonesian slang for "rubber time" — the flexible, approximate nature of time in Indonesian culture. In Gaul, `~=` brings this philosophy to comparisons.

### For Numbers: Within 5% Tolerance

```gaul
100 ~= 104    // true  (4% difference)
100 ~= 110    // false (10% difference)
3.14159 ~= 3.14  // true

// Perfect for floating-point comparisons
let Calculated = 0.1 + 0.2
Calculated ~= 0.3  // true (finally!)
```

### For Strings: Levenshtein Distance

```gaul
"hello" ~= "helo"     // true  (1 typo)
"hello" ~= "hallo"    // true  (1 substitution)
"hello" ~= "world"    // false (too different)

// Great for fuzzy matching user input
if(User Input ~= "yes") {
    println("Proceeding...")
}
```

### Configure Tolerance

```bash
# 10% tolerance for numbers
gaul-lang script.gaul --jam_karet_num 0.1

# 30% tolerance for strings
gaul-lang script.gaul --jam_karet_str 0.3
```

---

## Custom Keywords: Code in Your Language

Gaul's killer feature: define keywords in any language via a simple JSON file.

### French (français.gaul)

```gaul
// Lancez avec: gaul-lang script.gaul --keywords français.json

sortilège Fibonacci(N) {
    si(N <= 1) {
        renvoie N
    }
    renvoie Fibonacci(N - 1) + Fibonacci(N - 2)
}

soit Résultat = Fibonacci(10)
println(Résultat)
```

Where `français.json` maps:
```json
{
  "function": "sortilège",
  "return": "renvoie",
  "if": "si",
  "let": "soit",
  "true": "vrai",
  "false": "faux"
}
```

### Mandarin Chinese (中文.gaul)

```gaul
// 运行: gaul-lang script.gaul --keywords 中文.json

法术 斐波那契(第几个) {
    如果(第几个 <= 1) {
        返回 第几个
    }
    返回 斐波那契(第几个 - 1) + 斐波那契(第几个 - 2)
}

设 结果 = 斐波那契(10)
println(结果)
```

Where `中文.json` maps:
```json
{
  "function": "法术",
  "return": "返回",
  "if": "如果",
  "let": "设"
}
```

### Indonesian Slang (indonesia.gaul)

```gaul
// Jalankan: gaul-lang script.gaul --keywords indonesia.json

mantra Fizzbuzz Gaul(N) {
    anggep I = 1
    selama(I <= N) {
        biarin Bisa Bagi 3 = I % 3 == 0
        biarin Bisa Bagi 5 = I % 5 == 0

        kalo((Bisa Bagi 3) && (Bisa Bagi 5)) {
            println("✨ Mantul!")
        } eh {
            kalo(Bisa Bagi 3) {
                println("🔥 Gaskeun!")
            } eh {
                kalo(Bisa Bagi 5) {
                    println("💯 Cakep!")
                } eh {
                    println(I)
                }
            }
        }
        I = I + 1
    }
}

Fizzbuzz Gaul(15)
```

Indonesian keyword mapping:
```json
{
  "function": "mantra",
  "return": "balikin",
  "true": "yoi",
  "false": "kagak",
  "let": "biarin",
  "var": "anggep",
  "if": "kalo",
  "else": "eh",
  "while": "selama",
  "for": "untuk",
  "break": "cabut",
  "continue": "gas"
}
```

---

## Battle-Tested: Advent of Code 2025

Gaul has been used to solve **Advent of Code 2025 Days 1-12**, proving that while it's still a toy language at this point, it CAN already be used to solve problems. Here's a taste of Day 4's solution using functional programming:

```gaul
fn Count Adjacent Rolls(Grid, Row, Col) {
    let Offsets = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, -1],           [0, 1],
        [1, -1],  [1, 0],  [1, 1]
    ]
    (Offsets
        .map(fn(Offset) { Get Cell(Grid, Row + Offset.get(0), Col + Offset.get(1)) })
        .filter(fn(Cell) { Cell == "@" })
        .len())
}

fn All Positions(Grid) {
    let Rows = Grid.len()
    let Cols = Grid.get(0).len()
    (0..Rows).to_array().reduce([], fn(Acc, Row) {
        let Row Positions = (0..Cols).to_array().map(fn(Col) { [Row, Col] })
        Row Positions.reduce(Acc, fn(A, Pos) {
            A.push(Pos)
            A
        })
    })
}
```

Days 1-6 were written by hand, days 7-12 with LLM assistance. Check out `src/samples/aoc/2025/` for all the solutions.

Other samples:
- `src/samples/tasks/` — multi-file task manager demonstrating modules (import/export)
- `src/samples/map.gaul` — maps: frequency counting, visited sets, iteration
- `src/samples/closure.gaul` — closures and mutable captured state
- `src/samples/fib.gaul` — recursive Fibonacci (performance baseline)
- `src/samples/custom_keywords/` — French, Mandarin, and Indonesian keyword examples

---

## Rich Standard Library

### Strings
```gaul
// Escape sequences
let Line = "hello\nworld"       // newline
let Tabbed = "col1\tcol2"       // tab
let Quoted = "she said \"hi\""  // embedded double quote
let Path = "C:\\Users\\file"    // literal backslash
// Supported escapes: \n \t \r \\ \"
```

### String Methods
```gaul
let S = "Hello, World!"
S.len()              // 13
S.chars()            // ["H", "e", "l", "l", "o", ...]
S.substring(0, 5)    // "Hello"
S.split(", ")        // ["Hello", "World!"]
S.contains("World")  // true
S.trim()             // removes whitespace
S.to_num()           // parse to number
```

### Array Methods
```gaul
let Arr = [3, 1, 4, 1, 5, 9, 2, 6]
Arr.len()            // 8
Arr.first()          // 3
Arr.last()           // 6
Arr.sum()            // 31
Arr.min()            // 1
Arr.max()            // 9
Arr.sort()           // [1, 1, 2, 3, 4, 5, 6, 9]
Arr.sort_by(fn(A, B) { A - B })   // custom comparator
Arr.sort_by_key(fn(X) { X.abs() }) // sort by key
Arr.reverse()        // [6, 2, 9, 5, 1, 4, 1, 3]
Arr.flatten()        // [[1,2],[3]] → [1, 2, 3]
Arr.contains(5)      // true
Arr.join("-")        // "3-1-4-1-5-9-2-6"
Arr.zip([10, 20])    // [[3,10], [1,20]]
Arr.chunk(3)         // [[3,1,4], [1,5,9], [2,6]]
Arr.take(3)          // [3, 1, 4]
Arr.skip(3)          // [1, 5, 9, 2, 6]
Arr.insert(0, 99)    // inserts 99 at index 0 (mutates)
Arr.swap(0, 1)       // swaps elements at indices 0 and 1 (mutates)
Arr.any(fn(X) { X > 5 })       // true
Arr.all(fn(X) { X > 0 })       // true
Arr.count(fn(X) { X > 3 })     // 3
Arr.flat_map(fn(X) { [X, X] }) // [3,3,1,1,4,4,...]
```

### Map Methods
```gaul
let M = ["a": 1, "b": 2, "c": 3]
M.get("a")               // 1
M.get("z")               // null (missing key)
M.get_or("z", 0)         // 0 (with default)
M.set("d", 4)            // insert or overwrite
M.has("a")               // true
M.remove("c")            // 3 (returns removed value)
M.keys()                 // ["a", "b", "d"]
M.values()               // [1, 2, 4]
M.entries()              // [["a", 1], ["b", 2], ["d", 4]]
M.len()                  // 3
M.is_empty()             // false
```

### Number Methods
```gaul
let N = -42.7
N.abs()              // 42.7
N.floor()            // -43
N.ceil()             // -42
N.round()            // -43
N.pow(2)             // 1823.29
N.sqrt()             // NaN (negative)
42.floor_div(5)      // 8
```

### Number Literals
```gaul
let Decimal = 1_000_000    // 1000000 (separators for readability)
let Hex = 0xFF             // 255
let Binary = 0b1010        // 10
let Octal = 0o77           // 63
```

### Type Inspection
```gaul
type_of(42)          // "number"
type_of("hello")     // "string"
type_of(true)        // "bool"
type_of(null)        // "null"
type_of([1, 2])      // "array"
type_of(["a": 1])    // "map"
type_of(0..10)       // "range"
type_of(println)     // "function"
```

---

## Standard Library Modules

Gaul provides built-in modules that you import by name (no file path needed).

### `"math"` -- Trigonometry, Logarithms, Min/Max
```gaul
import { sin, cos, PI, min, max } from "math"

sin(PI / 2)          // 1
cos(0)               // 1
min(3, 5)            // 3
max(3, 5)            // 5
```

Available exports: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `log` (natural), `log2`, `log10`, `exp`, `min`, `max`, `PI`, `E`.

### `"fs"` -- File System
```gaul
import { read_file, write_file, file_exists } from "fs"

let Content = read_file("data.txt")
write_file("out.txt", "hello")
file_exists("data.txt")   // true
```

Available exports: `read_file`, `write_file`, `append_file`, `file_exists`.

### `"sys"` -- System Utilities
```gaul
import { clock, args, env_get } from "sys"

let T = clock()       // seconds since epoch
let Argv = args()     // command-line arguments as array
env_get("HOME")       // environment variable (null if unset)
```

Available exports: `args`, `exit`, `clock`, `env_get`, `env_set`.

---

## String Interpolation

`format(template, ...)` replaces `{}` placeholders with values:

```gaul
format("hello {}", "gaul")          // "hello gaul"
format("score: {}", 42)             // "score: 42"
format("{} + {} = {}", 1, 2, 3)     // "1 + 2 = 3"

var name = "world"
format("hello, {}!", name)          // "hello, world!"
```

The number of `{}` must match the number of extra arguments — mismatch is a runtime error.

---

## Modules

Split programs across multiple files using `export` and `import`.

```gaul
// math.gaul
export fn double(x) { x * 2 }
export let Pi = 3.14159
```

```gaul
// main.gaul
import { double, Pi } from "math.gaul"
println(double(21))   // 42
println(Pi)           // 3.14159
```

Rules:
- Only `export let`, `export var`, and `export fn` are valid — exporting expression statements is a parse error
- Imports are resolved relative to the importing file's directory (not the process CWD)
- Each module file is executed only once (cached after first import)
- Circular imports are detected and reported as a runtime error
- Imported names are immutable (`let`-style) bindings
- Custom keywords work here too — `"import"`, `"export"`, `"from"` can all be remapped

### Multi-file example

`src/samples/tasks/` is a working task manager split across four files:

```
tasks/
  main.gaul     — entry point
  list.gaul     — add, complete, remove, filter tasks
  stats.gaul    — completion rate and summary
  format.gaul   — pretty-printing helpers
```

```gaul
// list.gaul
export fn Make Task(Id, Title) {
    ["id": Id, "title": Title, "done": false]
}

export fn Add Task(Tasks, Title) {
    let Id = Tasks.len() + 1
    Tasks.push(Make Task(Id, Title))
    Tasks
}

export fn Complete Task(Tasks, Id) {
    for(Task : Tasks) {
        if(Task.get("id") == Id) { Task.set("done", true) }
    }
    Tasks
}

export fn Pending Tasks(Tasks) {
    Tasks.filter(fn(Task) { !Task.get("done") })
}
```

```gaul
// main.gaul
import { Add Task, Complete Task, Pending Tasks } from "list.gaul"
import { Summary, Completion Rate } from "stats.gaul"
import { Task Line, Section Header } from "format.gaul"

var Tasks = []
Tasks = Add Task(Tasks, "Design the module system")
Tasks = Add Task(Tasks, "Implement import and export")
Tasks = Add Task(Tasks, "Write integration tests")
Tasks = Add Task(Tasks, "Update documentation")

Tasks = Complete Task(Tasks, 1)
Tasks = Complete Task(Tasks, 2)
Tasks = Complete Task(Tasks, 3)

println(Section Header("All Tasks"))
for(Task : Tasks) { println(Task Line(Task)) }

println(format("  {}", Summary(Tasks)))
println(format("  Progress: {}%", Completion Rate(Tasks)))
```

Output:
```
--------------------------------------------
  All Tasks
--------------------------------------------
[x] 1    Design the module system
[x] 2    Implement import and export
[x] 3    Write integration tests
[ ] 4    Update documentation

  4  total  |  3  done  |  1  pending
  Progress: 75%
```

Run it: `gaul-lang src/samples/tasks/main.gaul`

---

## File I/O and Stdin

```gaul
import { read_file } from "fs"

let Content = read_file("data.txt")   // read entire file as string
let Lines = Content.lines()

for(Line : Lines) {
    println(Line.trim())
}
```

```gaul
let All = read_stdin()    // read all of stdin as a string
let Line = read_line()    // read one line (trailing newline stripped)
```

Works cross-platform (Linux, macOS, Windows).

```bash
# pipe input into a Gaul script
cat data.txt | gaul solve.gaul
echo "hello" | gaul greet.gaul
```

---

## Why Gaul?

**For educators:** Teaching programming concepts without the barrier of English keywords. Students can learn logic in their native language.

**For domain experts:** Write code that reads like your domain's terminology. Financial models with `Calculate VaR Adjustment`, medical software with `Check Patient Eligibility`.

**For the curious:** Challenge your assumptions about what programming languages can be. Why *shouldn't* identifiers have spaces?

**For practical use:** Fuzzy matching built into the language. No more `Math.abs(a - b) < epsilon` boilerplate.

---

## Implementation

Gaul is implemented in Rust with:
- Hand-written recursive descent parser
- Tree-walking interpreter
- Closure support with proper environment capture
- Levenshtein distance for fuzzy string matching (via `strsim`)
- REPL with history support (via `rustyline`)

---

## License

MIT

---

*"Gaul itu keren? Yoi!"* — Is Gaul cool? Absolutely!
