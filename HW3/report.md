# F1VAE Interpreter Implementation Report

2022315974 문장일

## 1. Overview

This report describes the implementation of an interpreter for F1VAE (Functions with 1 Variable and Arithmetic Expressions), a functional programming language that supports multiple functions with parameters, arithmetic operations, variable bindings, and function calls.

## 2. Language Features

F1VAE supports the following constructs:

- **Numbers**: Integer literals
- **Variables**: Identifier lookup
- **Arithmetic**: Addition and subtraction operations
- **Let-binding**: Local variable binding (`let x = e1 in e2`)
- **Functions**: Multiple function definitions with parameters
- **Function calls**: Call-by-value semantics with arity checking

## 3. Implementation Structure

### 3.1 Core Components

The interpreter consists of three main functions:

1. **`interp_expr`**: Evaluates expressions in a given environment
2. **`interp_fundef`**: Processes function definitions
3. **`interp`**: Main interpreter that orchestrates the evaluation

### 3.2 Environment Management

Two separate environments are maintained:

- **Function Store (Λ)**: Maps function names to their parameter lists and bodies
  - Type: `FStore.t = (string list * Ast.expr) Map`
  - Stores function definitions globally

- **Variable Store (σ)**: Maps variable names to their values
  - Type: `Store.t = Value.t Map`
  - Manages variable bindings during evaluation

## 4. Key Design Decisions

### 4.1 Call-by-Value Semantics

Function arguments are evaluated **before** the function body is executed. This ensures strict evaluation order:

1. Evaluate all arguments in the current environment
2. Create a fresh store with parameter bindings
3. Evaluate the function body in the new environment

### 4.2 Fresh Store for Function Calls

Each function call creates a **fresh variable store** containing only parameter bindings. This implements static scoping and ensures that:

- Functions cannot access variables from the caller's environment
- Only parameters and nested let-bindings are accessible
- No variable shadowing issues occur across function boundaries

### 4.3 Error Handling

Three types of runtime errors are detected:

1. **Free identifier**: Variable used without being bound
2. **Undefined function**: Calling a function that doesn't exist
3. **Arity mismatch**: Wrong number of arguments in function call

Error messages follow the specified format for automated testing.

## 5. Evaluation Rules Implementation

### 5.1 Basic Expressions

- **Num**: Returns the numeric value directly
- **Id**: Looks up the variable in the current store
- **Add/Sub**: Recursively evaluates both operands and performs the operation

### 5.2 Let-binding

Implements the rule:
```
Λ, σ ⊢ e1 ⇓ n1    Λ, σ[x ↦ n1] ⊢ e2 ⇓ n2
─────────────────────────────────────────
      Λ, σ ⊢ let x = e1 in e2 ⇓ n2
```

The implementation extends the current store with the binding before evaluating the body.

### 5.3 Function Call

Implements the rule:
```
Λ(f) = ([x1;...;xk], e)    Λ, σ ⊢ e1 ⇓ n1 ... Λ, σ ⊢ ek ⇓ nk
Λ, [x1↦n1;...;xk↦nk] ⊢ e ⇓ n
───────────────────────────────────────────────────────────
                Λ, σ ⊢ f(e1,...,ek) ⇓ n
```

Key steps:
1. Verify function existence
2. Check arity (parameter count = argument count)
3. Evaluate all arguments in the current environment
4. Bind parameters to argument values in a fresh store
5. Evaluate function body with the new bindings

## 6. Testing

All 15 test cases pass successfully, covering:

- Exception handling (free identifiers, undefined functions, arity mismatches)
- Function calls with various parameter counts
- Arithmetic operations with nested expressions
- Let-binding with variable scoping
- Multiple function definitions

## 7. Conclusion

The interpreter correctly implements the F1VAE language specification with:

- Proper call-by-value semantics
- Static scoping through fresh store creation
- Comprehensive error detection
- Clean separation of function and variable environments

The implementation follows the formal semantics rules and passes all test cases, demonstrating correctness and robustness.