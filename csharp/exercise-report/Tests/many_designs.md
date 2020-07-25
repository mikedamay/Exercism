## Learning objectives

- Know what a variable is
- Know how to define a variable
- Know how to update a variable
- Know how to use type inference for variables
- Know how to define a method
- Know how to return a value from a method
- Know how to call a method
- Know that methods must be defined in classes
- Know about the `public` access modifier
- Know about the `static` modifier (only in `after.md`)
- Know how to define an integer
- Know how to use mathematical operators on integers
- Know how to define single- and multiline comments

## Out of scope

- Naming rules for identifiers
- Generic values
- Memory and performance characteristics
- Method overloads
- Nested methods
- Lambda's
- Named parameters
- Optional parameters
- Classes
- Organizing methods in namespaces
- Visibility

## Concepts

- `basics`: know what a variable is; know how to define a variable; know how to update a variable; know how to use type inference for variables; know how to define a method; know how to return a value from a method; know how to call a method; know that methods must be defined in classes; know about the `public` access modifier; know about the `static` modifier; know how to define an integer; know how to use mathematical operators on integers; know how to define single- and multiline comments.

## Prerequisites

There are no prerequisites.

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the `RemainingMinutesInOven()` method calls the `ExpectedMinutesInOven()` method.
- Verify that the `ElapsedTimeInMinutes()` method calls the `PreparationTimeInMinutes()` method.

[analyzer]: https://github.com/exercism/csharp-analyzer

separator-1729
## Learning objectives

- Know how to clean up resources using `IDisposable` in C# for managed resources.
- Know how and when to implement the `IDisposable` interface on your own classes.

## Out of scope

- resource lifetime - `using` dealt with in the `resource-lifetime` exercise
- dispose pattern: handled in a separate exercise

## Concepts

- `resource-cleanup`: Know how to clean up resources with `IDisposable` in C# and .NET. Understand the difference between managedd and unmanaged resources and the role of `IDisposable`.

## Prerequisites

- `exceptions`
- `interfaces`: `IDisposable`

separator-1729
## Learning objectives

- Know of the existence of the `null` literal.
- Know what a `NullReferenceException` is and when it is thrown.
- Know how to compare a value to `null`.
- Know the difference between value and reference types regarding nullability, especially pre C# 8.0.
- Know how to define nullable reference and value types.
- Know about the null-related operators (`!`, `?`, `??`).
- Know about basic null checking by the compiler.

## Out of scope

- Nullable attributes.
- In-depth discussion of null checking by the compiler.
- Enabling C# 8 null checking.
- Casting using the `as` or `is` operator or using pattern matching.

## Concepts

- `nullability`: know of the existence of the `null` literal; know what a `NullReferenceException` is and when it is thrown; know how to compare a value to `null`; know the difference between value and reference types regarding nullability; know how to define nullable reference and value types; know about the null-related operators; know about basic null checking by the compiler.

## Prerequisites

- `strings`: strings will be compared to `null` and basic methods from strings will be called.
- `basics`: integers will be compared to `null`, arithmetic operations will be performed on integers, variables will be introduced and updated.
- `conditionals`: using a conditional statement.
- `memory-allocation`: reference and value types will be used in their nullable and non-nullable variants.

[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null

separator-1729
## Learning objectives

- Know of the existence of the `string` type.
- Know how to create a string.
- Know of some basic string methods (like finding the index of a character at a position, or returning a part the string).
- Know how to do basic string formatting.

## Out of scope

- Using standard or custom format strings.
- Memory and performance characteristics.
- Strings can be enumerated.

## Concepts

- `strings`: know of the existence of the `string` type; know of some basic functions (like looking up a character at a position, or slicing the string); know how to do basic string formatting.

## Prerequisites

This exercise's prerequisites Concepts are:

- `basics`: know how to define methods.

separator-1729
## Learning objectives

- Know how to implement randomness with `System.Random` in C#.
- Know that no seed is required
- Know that both integers and real numbers can be generated
- Know that once generated random numbers can be easily used for many purposes (including non-numeric ones).
- Know about thread safety and how it relates to the `System.Random` class (discussion only).

## Out of scope

- Secure random numbers

## Concepts

- `randomness`: know how to implement randomness with `System.Random` in C#; know that no seed is required; know that both integers and real numbers can be generated; know that once generated random numbers can be easily used for many purposes (including non-numeric ones)

## Prerequisites

- `numbers`

separator-1729
## Learning objectives

- Know how and why to use nested classes in C#.
- Know how and why to use nested structs (discussion only).

## Out of scope

- `namespaces`

## Concepts

- `nested-types`

## Prerequisites

- `classes`
- `structs`
- `enums`
- `properties`
- `interfaces`

separator-1729
## Learning objectives

- Know of the existence of the two most commonly used number types, `int` and `double`.
- Understand that an `int` represents whole numbers, and a `double` represents floating-point numbers.
- Know of basic operators such as multiplication, comparison and equality.
- Know how to convert from one numeric type to another; know what implicit and explicit conversions are.
- Know how to conditionally execute code using an `if` statement.

## Out of scope

- Any other numeric types besides `int` and `double` (so no `float`, `byte`, etc.).
- Parsing a `string` to an `int` or `double`.
- Converting an `int` or `double` to a `string`.

## Concepts

- `numbers`: know of the existence of the two most commonly used number types, `int` and `double`; understand that the former represents whole numbers, and the latter floating-point numbers; know of basic operators such as multiplication, comparison and equality; know how to convert from one numeric type to another; know what implicit and explicit conversions are.
- `conditionals`: know how to conditionally execute code using an `if` statement.

## Prerequisites

This exercise's prerequisites Concepts are:

- `basics`: know how to define methods.

separator-1729
## Learning objectives

- Know what generic types are.
- Know of the existence of the `List<T>` type.
- Know how a list is different from an array.
- Know how to define a list.
- Know how to add and remove elements from a list.
- Know how to access elements in a list by their index.
- Know how to iterate over elements in a list.
- Know some basic list functions (like finding the index of an element in a list or sorting a list).

## Out of scope

- Generic functions.
- Generic constraints.
- Memory and performance characteristics.
- LINQ.
- Concurrency issues.
- Co-/contravariance.
- Equality.
- List resizing due to it using an array as its data type.

## Concepts

- `lists`: know of the existence of the `List<T>` type; know how a list is different from an array; know how to define a list; know how to add and remove elements from a list; know how to access elements in a list by their index; know how to iterate over elements in a list; know some basic list functions (like finding the index of an element in a list).
- `generic-types`: know what generic types are.

## Prerequisites

- `for-loops`: know how to use a `for-loop` to iterate over a collection.
- `arrays`: contrast with dynamic nature of lists

separator-1729
## Learning objectives

- Know how to use the `ToString()` method to convert any object to a `string`.
- Know how to use string interpolation on values of any type.
- Know how to use default format strings to convert to standard output formats.
- Know how to use custom format strings to convert to custom output formats.
- Know that `string.Format` underlies string interpolation.
- Know of the `StringBuilder` type and when to use it.
- Know that string interpolation can interpolate any expression.

## Out of scope

`IFormatProvider`, `ICustomFormatter`

## Concepts

- `string-formatting`: know how to use the `ToString()` method to convert any object to a `string`; know how to use string interpolation on values of any type; know how to use default format strings to convert to standard output formats; know how to use custom format strings to convert to custom output formats; know that `string.Format` underlies string interpolation; know of the `StringBuilder` type and when to use it; know that string interpolation can interpolate any expression.
- `verbatim-strings`: the syntax of verbatim strings.

## Prerequisites

- `strings`: strings will be formatted.
- `inheritance`: knowing that each class derives from `object` and thus has built-in methods.
- `const-readonly`
- `time`: for use of `CultureInfo`.
- `varargs`: for the common overload of `public static string Format (string format, params object[] args);`

separator-1729
## Learning objectives

- Know what classes are.
- Know what encapsulation is.
- Know what fields are.
- Know how to create an object.
- Know how to update state through methods.
- Know about the `void` type.

## Out of scope

- Reference equality.
- Reference parameter passing.
- Constructors.
- Interfaces.
- Inheritance.
- Properties.
- Indexers.
- Structs.
- Destructors.
- Method overloading.
- Pattern matching.

## Concepts

- `classes`: know what classes are; know what encapsulation is; know what fields are; know how to create an object; know how to update state through methods; know about the `void` type.

## Prerequisites

- `basics`: know how to define a basic class with basic methods.
- `strings`: know how to do basic string interpolation.
- `numbers`: know how to compare numbers.
- `conditionals`: know how to do conditional logic.

separator-1729
## Learning objectives

- Know what a tuple is.
- Know how to define a tuple.
- Know how to name tuple fields.
- Know how to deconstruct tuples.
- Know that tuples are mutable.

## Out of scope

- The old `Tuple` class.
- Pattern matching on tuples.
- How to add tuple pattern matching to your own and built-in types.
- Know that tuples have structural equality.

## Concepts

- `tuples`: know what a tuple is; know how to define a tuple; know how to name tuple fields; know that tuples have structural equality; know how to deconstruct tuples; know that tuples are mutable.

## Prerequisites

- `basics`: know how to define methods and variables.
- `strings`: obtaining sub-strings from a string
- `conditionals`: combining conditions.

## Representer

See [this issue][representer-issue]: handle tuple field names correctly in the [representer][representer].

## Analyzer

See [this representer issue][representer-issue]: handle tuple field names correctly in the [analyzer][analyzer].

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[representer-issue]: https://github.com/exercism/csharp-representer/issues/3

separator-1729
## Learning objectives

- Know of the existence of the `char` type.
- Know what a char represents (a Unicode character).
- Know how to define a `char`.
- Know that a `char` is not the same as a single character string
- Know how to access a `char` in a string by index.
- Know of some basic `char` methods (like converting to uppercase).
- Know that `char`s are immutable.
- Know how to compare characters
- Know how to use a `StringBuilder`

## Out of scope

- Converting an integer to a character and vice versa.
- `System.Char` as a struct - alias for the simple `char` type
- Advanced unicode issues such as surrogates, text normalization, combining characters
- cultural considerations and invariants

## Concepts

- `chars`: know of the existence of the `char` type; know that a `char` represents; know how to define a `char`; know how to access a `char` in a string by index; know of some basic `char` methods (like converting to uppercase).
- `StringBuilder`: know how to use this.

## Prerequisites

- `strings`: know of the `string` type that will be iterated over and accessed by index.
- `for-loop` for loops (rather than foreach) are the best means of highlighting the relationship between strings and `char`s

separator-1729
## Learning objectives

After completing this exercise, the student should:

- Know of the existence of the `enum` keyword.
- Know how to define enum members.
- Know how to assign values to enum members.
- Know how to get an enum's numeric value.
- Know how to use the `switch` statement to do constant pattern matching.

## Out of scope

- Flag enums.
- That an enum's underlying type can be changed.
- Memory and performance characteristics.
- Parse `string` into an `enum`.

## Concepts

- `enums`: know of the existence of the `enum` keyword; know how to define enum members; know how to assign values to enum members; know how to get an enum's numeric value; know how to convert an `enum` to a `string`.
- `pattern-matching-constants`: know how to use the `switch` statement to do constant pattern matching.

## Prerequisites

This exercise's prerequisites Concepts are:

- `strings`: log lines are `string` values.
- `conditionals`: know how to execute conditional logic.

[docs.microsoft.com-enum]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/enum

separator-1729
## Learning objectives

- Know of the existence of the `bool` type and its two values.
- Know about boolean operators and how to build logical expressions with them.
- Know of the boolean operator precedence rules.

## Out of scope

- Pattern matching on booleans.

## Concepts

- `booleans`: know of the existence of the `bool` type and its two values; know about boolean operators and how to build logical expressions with them; know of the boolean operator precedence rules.

## Prerequisites

This exercise's prerequisites Concepts are:

- `basics`: know how to define methods.

separator-1729
## Learning objectives

- Know how to use simple `switch` statements.
- Know how to include pattern matching on types.
- Know how to include guards

## Out of scope

- switch expressions
- pattern matching tuples

## Concepts

- `switch-statements`: Know how to use switch statements

## Prerequisites

- `enums`
- `classes`
- `inheritance`: with type pattern matching the student needs to be aware of the need for a common base type including `Object`.

separator-1729
## Learning objectives

- Know how to use `time` in C#.
- Know how to get the current time.
- Know how to perform arithmetic on times.
- Know the difference between local time and UTC.

## Out of scope

- custom time zones
- [Noda Time][noda-time]
- We are not exercising student's ability to code a cross-platform solution.

## Concepts

- `time`: Know how to use `DateTime` when time-of-day is important. Understand the difference between local time and Universal Coordinated Time). Understand the role of `CultureInfo` in parsing times. Understand arithmetic with `DateTime`s.
- `timezone`: Know about time zones and their ids. Be familiar with cross-platform issues. Know how to convert dates and times between time zones. Know how to detect daylight saving time.
- `cross-platform`: know how to have different code paths (selected at run-time) for Linux, Windows and Mac

## Prerequisites

- `datetime`
- `switch-statements`
- `strings`
- `conditionals-if`

[noda-time]: https://nodatime.org/

separator-1729
## Learning objectives

- Know what namespaces are.
- Know how to import namespaces.

## Out of scope

- Assemblies.

## Concepts

- `namespaces`: know what namespaces are; know how to import namespaces.
- `accessibility`: know how to use access modifiers to limit access to elements.

## Prerequisites

- `classes`: know how to define a class and what the `public` and `private` access modifiers are.
- `inheritance`: know about the `protected` access modifier.
- `nested-types`

separator-1729
## Learning objectives

- Know what constructors are
- Know how to define parameterless constructors
- Know how to define parameterized constructors

## Out of scope

- Constructor overloading
- Private constructors
- Static constructors
- Destructors

## Concepts

- `constructors`: know what constructors are; know how to define parameterless constructors; know how to define parameterized constructors; know how to use constructor overloading; know how to define private constructors.

## Prerequisites

- `classes`: know how to work with classes.
- `numbers`: know how compare numbers.
- `conditionals`: know how to do conditional logic.
- `while-loops`: know how to use `while` loops.

separator-1729
## Learning objectives

- Know how to check for equality and inequality.
- Know that equality works by default for value and reference types.
- Know how to customize equality checks using `Equals` and `GetHashCode()`.
- Know of the `IEquatable<T>` and `IEqualityComparer<T>` interfaces and how to implement them (discussion only).
- Know how to use `System.Collections.Generic.HashSet` and its relationship to `GetHashCode()`.

## Out of scope

- Collection equality.
- `struct` equality
- Equality of primitives

## Concepts

This Concepts Exercise's Concepts are:

- `equality`: know how to check for equality and inequality; know how reference equality differs from structural equality; know that equality works by default for value and reference types; know how to customize equality checks using `Equals` and `GetHashCode()`; know of the `IEquatable<T>` and `IEqualityComparer<T>` interfaces and how to implement them.
- `sets`: Know how to use hash sets `HashSet<T>` as provided by the .NET BCL. Understand the relationship with `Object.GetHashCode()` and the performance charateristics of hash sets.

## Prerequisites

This Concept Exercise's prerequisites Concepts are:

- `generic-types`: needed for understanding the `IEquatable<T>` interface.
- `interfaces`: know how to implement interfaces
- `inheritance`: know that all types are derived from `object`.
- `classes`: know how to define and work with classes.
- `lists`: Know what a collection looks like and how it is generally used.
- `explicit-casts`: object -> T

separator-1729
## Learning objectives

- Know what method overloading is
- Know how to define overloaded methods
- Know the limitations of method overloading
- Know how to define optional parameters
- Know how to pass named arguments

## Out of scope

- Overload resolution

## Concepts

- `method-overloading`: know what method overloading is; know how to define overloaded methods; know the limitations of method overloading
- `optional-parameters`: know how to define optional parameters
- `named-arguments`: know how to use named arguments

## Prerequisites

- `classes`: know how to define methods on classes
- `constructors`: know how to define constructors on classes
- `properties`: know how to work with properties
- `enums`: know how to use enums
- `strings`: know how to format strings
- `basics`: know how to work with integers

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the `Describe()` methods take take both a `Character` and `Destination` can be replaced with a single method that uses a default value for the `TravelMethod` parameter.

[analyzer]: https://github.com/exercism/csharp-analyzer

separator-1729
## Learning objectives

- Explain integral number overflows.
- Explain floating-point number overflows.
- Know how to use `checked` and `unchecked` to deal with overflows.

## Out of scope

- BigInteger

## Concepts

- `overflows`: explain integral number overflows; explain floating-point number overflows; know how to use `checked` and `unchecked` to deal with overflows.

## Prerequisites

- `integral-numbers`: know about the integral number types.
- `floating-point-numbers`: know about the floating-point number types.
- `exceptions`: explain how an `OverflowException` is thrown when using checked arithmetic.
- `strings`: numbers are converted to strings.

separator-1729
## Learning objectives

- Know what inheritance is.
- Know how to inherit from a class.
- Know that all types inherit from `object`.
- Know what abstract and sealed classes are.
- Know what abstract and virtual methods are.
- Know how to override methods.
- Know about the `protected` visibility modifier.

## Out of scope

- Extending types through extension methods.

## Concepts

- `inheritance`: know what inheritance is; know how to inherit from a class; know that all types inherit from `object`; know what abstract and sealed classes are; know what abstract and virtual methods are; know how to override methods; know about the `protected` visibility modifier.

## Prerequisites

- `classes`: know how to work with classes.
- `constructors`: know how to work with constructors.
- `strings`: know how to do basic string interpolation.
- `boolean`: know how to use boolean logic.
- `conditionals`: know how to do conditional logic.

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the constructor of the `Character` class uses the `protected` modifier.
- Verify that the various fields used (hit points, spell prepared and potion drunk) use the `private` modifier.

[analyzer]: https://github.com/exercism/csharp-analyzer

separator-1729
Properties are covered early in the C# track as their purpose and power can be shown with few dependencies (classes, access modifiers and fields of simple types).

## Learning objectives

- Know what properties are and how they relate to fields and methods.
- Know what backing-field properties are.
- Know what auto-implemented properties are.
- Know what calculated properties are.
- Know how to use property accessors to customize visibility.
- Know how to define the different types of properties.

## Out of scope

- expression bodied properties, get accessors and set accessors (covered by expression-bodied members)
- properties on interfaces (covered by Interfaces)
- properties/absract properties on abstract classes (covered by Inheritance)
- use of the `readonly` keyword with properties (covered by Immutability)
- static properties (covered by Statics)
- indexers (covered by Indexers)

Note that students may choose to implement expression-bodied members.

## Concepts

- `properties`: know what properties are and how they relate to fields and methods; know what backing-field properties are; know what auto-implemented properties are; know what calculated properties are; know how to use property accessors to customize visibility; know how to define the different types of properties.

## Prerequisites

- `numbers`: using the `int` type and using mathematical operators and number conversions.
- `floating-point-numbers`: using the `decimal` type.
- `classes`: defining classes and working with members.
- `enums`: working with enums.
- `exceptions`: throwing an exception.

Note that the values in the instructions' examples and tests
are selected specifically to avoid any question of rounding when converting
between float and int. Rounding and truncation will produce the
same result.

Prerequisite Exercises - TBA

## Resources to refer to

### Hints

- [Properties][docs.microsoft.com-properties]
- [Using Properties][docs.microsoft.com-using-properties]

### After

- [Properties][docs.microsoft.com-properties]
- [Using Properties][docs.microsoft.com-using-properties]

As this is an introductory exercise, we should take care not to link to very advanced resources, to prevent overwhelming the student.

## Representer

TBC

## Analyzer

It is difficult to get the student to exercise all different aspects of
properties through tests alone. We need comments to address the following
practices:

1. If `WeighingMachine.Units` is not auto-implemented
   then the following comment should be made: "The appropriate form
   for a property such as `WeighingMachine.Units` which has no validation or other processing required is
   that for an auto-implemented property". - Approved with comment.

2. If `WeighingMachine.DisplayWeight` has a non-private set accessor
   then the following comment should be made: "It is not approprirate
   for a property such as `WeighingMachine.DisplayWeight` which simply returns a value
   to have a set accessor. That should be removed.". - Approved with comment.

3. If `WeighingMachine.USDisplayWeight` has a non-private set accessor
   then the following comment should be made: "It is not approprirate
   for a property such as `USWeighingMachine.DisplayWeight` which simply returns a value
   to have a set accessor. That should be removed.". - Approved with comment.

4. If `USDisplayWeight.Pounds` has a non-private set accessor
   then the following comment should be made: "It is not approprirate
   for a property such as `USDisplayWeight.Pounds` which simply returns a value
   to have a set accessor. That should be removed.". - Approved with comment.

5. If `USDisplayWeight.Ounces` has a non-private set accessor
   then the following comment should be made: "It is not approprirate
   for a property such as `USDisplayWeight.Ounces` which simply returns a value
   to have a set accessor. That should be removed.". - Approved with comment.

6. If `WeighingMachine.TareAdjustement` is not an auto-implemented property
   then the following commen should be made: "A succinct way of implementing
   `WeighingMachine.TareAdjustment` is as an auto-implemented property with a
   `private` get accessor". - Approved with comment.

7. If `WeighingMachine.TareAdjustment` is an auto-implemented property
   but the get accessor is non-private then the following comment should be made:
   "A non-private set accessor is not appropriate for `WeighingMachine.TareAdjustment`
   as the instructions stipulate that the value must not be available outside the
   class". - Disapproved.

## Implementing

If you'd like to work on implementing this exercise, the first step is to let us know through a comment on this issue, to prevent multiple people from working on the same exercise. If you have any questions while implementing the exercise, please also post them as comments in this issue.

Implementing the exercise means creating the following files:

<pre>
languages
└── csharp
    └── exercises
        └── concept
            └── properties
                ├── .docs
                |   ├── after.md
                |   ├── hints.md
                |   ├── instructions.md
                |   └── introduction.md
                ├── .meta
                |   ├── design.md
                |   └── Example.cs
                ├── Properties.csproj
                ├── Properties.cs
                └── PropertiesTest.cs
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it. The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student.

## Step 4: add .docs/after.md

Once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.

These files are also all described in the [concept exercises document][docs-concept-exercises].

## Step 5: update languages/csharp/config.json

An entry should be added to the track's `config.json` file for the new concept exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "properties",
        "uuid": "978bcc16-0c49-4328-92e9-79f6204ce350",
        "concepts": ["properties"],
        "prerequisites": [
          "numbers",
          "floating-point-numbers",
          "classes",
          "enums",
          "exceptions"
        ]
      }
    ]
  }
}
```

## Step 6: adding track-specific files

These files are specific to the C# track:

- `Properties.csproj`: the C# project file.
- `PropertiesTest.cs`: the test suite.
- `Properties.cs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `.meta/Example.cs`: an example implementation that passes all the tests.

Check out the [`floating-point-numbers exercise`][csharp-docs-concept-exercises-numbers-floating-point] for an example on what these files should look like.

## Step 7: update the general concept document

Not applicable for this concept

## Step 8: updating list of implemented exercises

- Add the exercise to the [list of implemented exercises][csharp-docs-concept-exercises].

## Step 9: add .meta/design.md:

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more ([example][meta-design]). This information can be extracted from this GitHub issue.

### Inspiration

When implementing this exericse, it can be very useful to look at already implemented C# exercises like the [strings][csharp-docs-concept-exercises-strings], [dates][csharp-docs-concept-exercises-datetimes] or [floating-point numbers][csharp-docs-concept-exercises-numbers-floating-point] exercises.

[docs.microsoft.com-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties
[docs.microsoft.com-using-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/using-properties
[docs.microsoft.com-foreach-with-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/using-foreach-with-arrays
[docs.microsoft.com-single-dimensional-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays
[docs.microsoft.com-implicitly-typed-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/implicitly-typed-arrays
[docs-v3]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#exercise-structure
[docs-v3-types-array]: https://github.com/exercism/v3/blob/master/reference/types/array.md
[docs-v3-types-collection]: https://github.com/exercism/v3/blob/master/reference/types/collection.md
[csharp-docs]: https://github.com/exercism/v3/blob/master/languages/csharp/README.md
[csharp-docs-concept-exercises-strings]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/strings
[csharp-docs-concept-exercises-datetimes]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/dates
[csharp-docs-concept-exercises-numbers-floating-point]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/floating-point-numbers
[csharp-analyzer]: https://github.com/exercism/csharp-analyzer
[csharp-representer]: https://github.com/exercism/csharp-representer
[csharp-docs-cli.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/.docs/cli.md
[csharp-docs-debug.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/.docs/debug.md
[csharp-docs-after.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/after.md
[csharp-docs-hints.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/hints.md
[csharp-docs-introduction.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/introduction.md
[csharp-docs-instructions.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/instructions.md
[csharp-docs-design.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/design.md
[csharp-meta-config.json]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.meta/config.json
[csharp-docs-concept-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md
[referrence-array]: https://github.com/exercism/v3/blob/master/reference/types/array.md

separator-1729
## Learning objectives

- Know what structs are.
- Know how to define a `struct`.
- Know how to add members to structs.
- Know the differences between structs and classes.

## Out of scope

- ref structs.
- ref returns
- ref locals
- readonly structs
- Struct finalizers.
- `Span<T>`/`Memory<T>`
- `stackalloc`

## Concepts

This Concepts Exercise's Concepts are:

- `structs`: know what structs are; know how to define a `struct`; know how to add members to structs; know the differences between structs and classes.

## Prerequisites

- `inheritance`: know that all types are derived from `object`.
- `classes`: know how to define and work with classes.
- `numbers`:
- `sets`: know how to define and work with a `HashSet`
- `signed-integers`/`integral-numbers`: for `ushort`

separator-1729
## Learning objectives

- Know of the existence of the `DateTime` type.
- Know how to create a `DateTime` instance.
- Know how to get the current date.
- Know of the individual, date-related properties.
- Know how to access the current date.
- Know how to compare dates.
- Know how to convert a `string` to a `DateTime` and vice versa.

## Out of scope

- Using standard or custom format strings.
- Everything related to timezones.
- Exact parsing using format strings.
- The `DateTimeOffset` type.
- The `TimeSpan` type.

## Concepts

- `datetime`: know how to create a `DateTime` instance; know how to get the current date; know of the individual, date-related properties; know how to access the current date; know how to compare dates; know how to convert a `string` to a `DateTime` and vice versa; know of the existence of the `DateTime` type; know of the individual, time-related properties.

## Prerequisites

This exercise's prerequisites Concepts are:

- `numbers`: comparing the hour against specific number values.
- `strings`: dates are parsed from and converted to strings.
- `classes`: know how to call a constructor.

[docs.microsoft.com-datetime]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1

separator-1729
Of the many available C# collection types, we chose to use the `array` collection type as the first collection type students will be taught for the following reasons:

- Arrays don't require the student to know about generics.
- Arrays are a common data type in many languages.
- Arrays have a fixed length. No complexity in adding or removing elements.
- Arrays have a simple shorthand syntax. No need to understand how constructors work to define an array.

## Learning objectives

- The existence of the `Array` type.
- Defining an array.
- Accessing elements in an array by index.
- Updating an element in an array by index.
- Iterating over elements in an array.
- Basic array functions (like finding the index of an element in an array).

## Out of scope

- Multi-dimensional/jagged arrays.
- Memory and performance characteristics of arrays.
- Enumerables.
- Iterators.
- LINQ.

## Concepts

- `arrays`: know of the existence of the `Array` type; know how to define an array; know how to access elements in an array by index; know how to update an element in an array by index; know how to iterate over elements in an array; know of some basic functions (like finding the index of an element in an array).
- `for-loops`: know how to use a `for` loop to do iteration.
- `foreach-loops`: know how to iterate over a collection.

## Prerequisites

This exercise's prerequisites Concepts are:

- `classes`: know how to work with fields.
- `booleans`: know what a `bool` is.
- `basics`: know how to work with `integers` and how to assign and update variables.

separator-1729
## Learning objectives

- Know the difference between value and reference type parameters.
- Know how to pass value types by reference using the `ref` and `out` modifiers.

## Out of scope

- Overload resolution.
- ref returns.
- ref locals.
- `in` modifier

## Concepts

This Concepts Exercise's Concepts are:

- `parameters`: know the difference between value and reference type parameters; know how to pass value types by reference using the `ref` and `out` modifiers.

## Prerequisites

This Concept Exercise's prerequisites Concepts are:

- `conditionals`: `if`, `else`
- `numbers`
- `strings`
- `constructors`: the exercise provides a gentle intuitive introduction to simple parameters
- `named-parameters`: introduced in the `method-overloading` exercise and used here in _instructions.md_.

separator-1729
## Learning objectives

- Know how to use regular expressions with `Regex` in C#.
- Know how to identify the presence of a pattern in a string.
- Know how to "capture" and replace text identified by patterns.
- Know how to use the `Options` property of `Regex`.
- Know that search performance can be enhanced by compiling `Regex` (discussion only)

## Out of scope

- Intermediate and advanced regular expression patterns.

## Concepts

- `regular-expressions` as they are handled in C# and .NET.

## Prerequisites

- `strings`
- `arrays`
- `for-loops`
- `verbatim-strings`
- `string-interpolation`

separator-1729
## Learning objectives

- Know of the existence of the three floating point types: `double`, `float` and `decimal`.
- Know when to use which floating point type.
- Know how to write a `while` loop.

## Out of scope

- Parsing floating-point types from strings.
- Converting floating-point types to strings.
- Using standard or custom format strings.

## Concepts

- `floating-point-numbers`: know of the existing of the three floating point types: `double`, `float` and `decimal`. know when to use which floating point type.
- `while-loops`: know how to write a `while` loop.

## Prerequisites

This exercise's prerequisites Concepts are:

- `numbers`: define numbers and apply arithmetic and boolean logic to them.
- `conditionals`: conditionally execute code based on value of floating-point numbers.

[docs.microsoft.com-floating-point-numeric-types]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/floating-point-numeric-types

separator-1729
## Learning objectives

- Know how and when to user object initializers.
- Understand that object initializers are used with collections

## Out of scope

- Anonymous classes

## Concepts

- `object-initializers`: Know how to initialize objects using object initialization syntax. Know how to initialize lists and dictionaries. Understand the relative advantages of constructors and initializers.

## Prerequisites

- `constructors`
- `properties`
- `dictionaries`

separator-1729
## Learning objectives

- Know of the existence of the `Dictionary<TKey, TElement>` type.
- Know how to define a dictionary.
- Know how to add and updated elements in a dictionary.
- Know how to access elements in a dictionary by key.
- Know how to iterate over elements in a dictionary.
- Know some basic dictionary functions (like checking if a key exists).

## Out of scope

- Generic functions.
- Generic constraints.
- Memory and performance characteristics.
- LINQ.
- Concurrency issues.
- Co-/contravariance.
- Equality.
- The `Lookup<TKey, TElement>` type.

## Concepts

- `dictionaries`: know of the existence of the `Dictionary<TKey, TElement>` type; know how to define a dictionary; know how to add and updated elements in a dictionary; know how to access elements in a dictionary by key; know how to iterate over elements in a dictionary; know some basic dictionary functions.

## Prerequisites

- `foreach-loops`: know how to use a `foreach-loop` to iterate over a collection.
- `generic-types`: know how generic types work.
- `strings`: know how to discover string length
- `indexers`: usage and behavior of indexer properties
- `object-initializers`: how object initializers are used with collections.

[how-to-implement-a-concept-exercise]: https://github.com/exercism/v3/blob/master/docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[implemented-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md#implemented-exercises
[reference]: https://github.com/exercism/v3/blob/master/languages/csharp/reference/README.md#reference-docs
[reference-dictionary]: https://github.com/exercism/v3/blob/master/reference/types/dictionary.md
[reference-example]: https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations
[exercise-example]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/numbers-floating-point
[design-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/design.md
[config.json-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/config.json
[concept-exercises]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md
[dictionaries-docs]: https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.dictionary-2?view=netcore-3.1
[dictionaries-tutorial]: https://csharp.net-tutorials.com/collections/dictionaries/

separator-1729
## Learning objectives

- Know how to control a resource's lifetime with the `using` statement in C#.

## Out of scope

## Concepts

- `resource-lifetime` as it is handled in C#.

## Prerequisites

- `resource-cleanup`: this exercise highlights the difference between the two approaches,

[using-statement]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-statement
[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer

separator-1729
## Learning objectives

- Know what a flags enumeration is.
- Know how to define a flags enumeration.
- Know how to check if a flag has been set on an enum value.
- Know how to set a flag on an enum value.
- Know how to unset a flag on an enum value.
- Know that an enum's underlying type can be changed.
- Know how to use bitwise operators to manipulate bits.

## Out of scope

As this is an advanced exercise, there are no enum-related things that we should explicitly _not_ teach.

## Concepts

- `flag-enums`: know how to define a "flags" enum; know how to add, remove or check for flags; know how to change the underlying type of an enum.
- `bit-manipulation`: know how to use bitwise operators to manipulate bits.

## Prerequisites

This exercise's prerequisites Concepts are:

- `enums`: know how to define the `enum`.
- `attributes`: know how to annotate the enum with the `[Flags]` attribute.
- `integers`: know of other integer types than `int` and know about binary integer literals.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from having an [analyzer][analyzer] that can comment on:

- Verify that the `Permission` enum is marked with the `[Flags]` attribute.
- Suggest using `byte` as the enum's backing type if no backing type was explicitly specified.

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[docs.microsoft.com-enumeration-types-as-bit-flags]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/enumeration-types#enumeration-types-as-bit-flags
[docs.microsoft.com-bitwise-and-shift-operators]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators
[docs.microsoft.com-switch-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch
[docs.microsoft.com-binary-notation]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types#integer-literals
[docs.microsoft.com-flagsattribute]: https://docs.microsoft.com/en-us/dotnet/api/system.flagsattribute?view=netcore-3.1
[alanzucconi.com-enum-flags-and-bitwise-operators]: https://www.alanzucconi.com/2015/07/26/enum-flags-and-bitwise-operators/
[concept-bitwise-manipulation]: ../../../../../reference/concepts/bitwise_manipulation.md

separator-1729
## Learning objectives

- Know what explicit and implicit casts are.
- Know how to do an explicit cast.
- Know how to use `is` and `as` to cast types.
- Know how to use the `typeof` operator.
- Pattern matching on types.

## Out of scope

- Custom explicit and implicit cast operators.
- Memory and performance characteristics.

## Concepts

- `casting`: know what explicit and implicit casts are; know how to do an explicit cast; know how to use `is` and `as` to convert between types.

## Prerequisites

- `numbers`: know how to cast numbers and what explicit and implicit numeric casts are.
- `inheritance`: work with inheritance to show when casting can be reasonably used.
- `exceptions`: know about exceptions to understand what happens when an explicit cast fails.

separator-1729
## Learning objectives

- Know how to define a user-defined exception.
- Know how to use exception filtering.
- Know that using errors as control logic is an anti-pattern

## Out of scope

- Memory and performance characteristics.

## Concepts

This Concepts Exercise's Concepts are:

- `user-defined-exceptions`: know how to define a user-defined exception.
- `exception-filtering`: know how to use exception filtering.

## Prerequisites

This Concept Exercise's prerequisites Concepts are:

- `exceptions`: know how to work with exceptions.
- `inheritance`: inheriting from the `Exception` class for the custom exception.
- `strings`: converting an into a string
- `conditionals`: use of simple `if`/`else`
- `arithmetic-overflow`
- `signed-integers`: `Int32.MaxValue`
