# Programming Paradigms Project - BizLang Compiler

**Module 8 - Programming Paradigms, Bachelor of Technical Computer Science, University of Twente**

**Date:** 08-07-2024

**Contributors:**  
- [Dragoș Erhan](https://github.com/Headpoint2042)
- [Ion Tulei](https://github.com/iontulei)


**This project is based on the [**Simple Processor in Haskell**](https://github.com/bobismijnnaam/sprockell)**

## Project Overview

The objective of this project was to design and implement a **compiler** for a new programming language, **BizLang** (Bizarre Language). The entire compiler was written in **Haskell** and designed to generate code for a custom virtual machine called [**SPROCKELL**](https://github.com/bobismijnnaam/sprockell). This was done as part of the **Programming Paradigms** module in the **Technical Computer Science Bachelor's program** at the University of Twente.

We have develop both the **frontend** (parsing, type checking) and the **backend** (code generation) of a compiler, while adhering to certain **mandatory features** like supporting concurrency, basic data types, expressions, and control flow constructs.

## Language Features

### 1. **Data Types**

BizLang supports several primitive data types:

- **Integers** (`int`), which are used for mathematical operations and default to `0`.

- **Booleans** (`bool`), which are used for logical conditions and default to `false`.

- **Characters** (`char`), which default to a space character `' '`.

    These data types ensure that BizLang can handle typical operations found in modern programming languages, such as arithmetic, logic, and text manipulation.

### 2. **Expressions**

The language supports basic expressions that can be used to manipulate the aforementioned data types:

- **Arithmetic Expressions:** Addition (`+`), subtraction (`-`), multiplication (`*`), and signed integer division (`/`).

- **Logical Expressions:** AND (`&&`), OR (`||`), and NOT (`!`).

- **Relational Operators:** Greater than (`>`), less than (`<`), equality (`==`), inequality (`!=`), and others.

    Expressions can be combined and evaluated in both mathematical and conditional contexts.
    
### 3. **Statements**

BizLang supports several key statements essential for building structured programs:

- **Variable Assignment:** Variables can be assigned the result of expressions.

- **Conditional Statements:** `if-else` constructs for branching logic based on conditions.

- **Loops:** `while` loops for repeated execution of a block of code while a condition is true.

- **Print Statements:** Ability to print variables, expressions, and **string literals** to the console.

### 4. **Concurrency**

One of the more advanced features of BizLang is its built-in support for **concurrency** using a **fork-join** model. This allows programmers to spawn new threads of execution and synchronize them using **locks**. BizLang enables the creation of parallel programs that execute across multiple threads, with shared memory access controlled by explicit `global` variable declarations and lock mechanisms to prevent data races.

The language allows for the
declaration of new threads with the special `thread` keyword followed by braces
which denote the block of code to be executed by this thread. The main program does not execute the code inside this thread block. The language allows for nested declaration of threads, declaration of threads on all scope levels, and declaration of threads inside `if-else` blocks. However, it restricts the declaration of threads inside `while`
loops (due to limited shared memory).

For managing access to shared variables, **locks** are implemented. These are boolean values that are stored in the shared memory. The compiler makes use of the “TestAndSet” Sprockell instruction to modify them.

We did not implement a special **join** functionality. Instead, we use locks as a barrier. Because we do not have reentrant locks, we can use one lock to act as a barrier for all threads (including the main thread).
    
```c
Lock barrier;
barrier.lock;
thread {
    // parallel code
    barrier.unlock;
}
// main waits for thread to finish (aka join)
barrier.lock;
// following code
```

There exist two types of variables, **local** and **shared**. We use the special keyword `global` when declaring a variable to indicate that it should be declared on the memory shared between all threads. Locks are an exception as they are always global. The back-end diferentiates local and shared variables by storing them in two dierent lookup tables (one for local variables, and another for global variables).

The following sample code displays how **global (shared memory) variables**, **locks** and **threads** are created and used.

```c
Lock l;
global x;
thread {
    l.lock;
    x = x + 1;
    l.unlock;
}
```

Concurrency was implemented using SPROCKELL’s built-in thread management and synchronization instructions, making BizLang capable of executing multi-threaded programs efficiently.

### 5. **Bonus Feature: Integer Division**

BizLang also implements **signed integer division** as an additional feature. It handles both positive and negative division, with special cases for division by zero handled by the compiler.

## Compiler Architecture

The BizLang compiler follows a classic **three-phase architecture:** **Parsing**, **Elaboration** (type checking), and **Code Generation**. All phases were implemented in **Haskell**.

- `src/MyParser.hs`: Parsing functions.

- `src/MyElaborator.hs`: Elaboration and type-checking functions.

- `src/MyCodeGen.hs`: Code generation for Sprockell architecture.

### 1. Parsing

The parser is built using the **ParSec** library, which transforms the input source code into an **Abstract Syntax Tree (AST)**. This phase analyzes the syntax of the program and breaks it down into manageable constructs such as expressions, statements, and blocks of code. It ensures that the input conforms to the BizLang grammar, generating appropriate error messages when violations are encountered.


Key responsibilities of the parser include:

- Breaking down expressions into terms and factors.

- Identifying statements like `if-else`, `while`, `print`, and assignments.

- Handling variable declarations and scope management.

### 2. Elaboration

After parsing, the elaboration phase ensures the correctness of the program’s semantics. This is done by:

- **Type checking:** Ensuring that variables are assigned compatible values, and that operations like addition or comparison are performed on the correct types (e.g., integers or booleans).

- **Scope management:** Keeping track of variable declarations and ensuring that variables are initialized before use.

- **Shadowing:** Handling the redeclaration of variables in nested scopes and ensuring the correct variable is referenced.

We implemented the type checker using a **symbol table** (hashmap) that tracks each variable's type and scope.

### 3. Code Generation

The final phase translates the AST into **SPROCKELL** instructions. SPROCKELL is a custom architecture used in the course, and the output of the compiler is a sequence of **SPRIL** (SPROCKELL's assembly language) instructions that can be executed on the SPROCKELL simulator.

Responsibilities of code generation include:

- Translating expressions and conditions into corresponding SPRIL instructions.

- Generating code for control flow constructs (`if-else`, `while`).

- Handling thread creation and synchronization for concurrent programs.

- Managing memory for both local and global variables, including shared memory access.

## Compiling

To compile the project, open a terminal, navigate to the project directory, and run the following command to build the project:

```bash
stack build
```

This will install a local version of GHC (the Haskell compiler) and the required libraries. The files in the `src` and `app` directories will be compiled.

## Running the Compiler

To run the compiler on a BizLang source file, use the following command in the  terminal:

```bsah
stack run -- "path/to/source/file"
```

This will compile the source program and generate SPRIL code for execution on the SPROCKELL simulator.

## Automated Tests

We have implemented automated tests for each stage of the compilation process (parsing, elaboration, and code generation) using **HSpec** and **QuickCheck**. To run the tests, use:

```bash
stack test
```

This will execute all tests as defined in the `test` section of the `package.yml` file.

## Sample Programs

### 1. Trivial Program
```c
int x = 10;
bool isEven = true;

if (isEven) {
    print("Even number");
} else {
    print("Odd number");
}

while (x > 0) {
    print(x);
    x = x - 1;
}
```

To run this sample program, assuming it was saved in a known location, use the following command:

```bash
stack run -- "path/program.txt"
```

Expected output:
```bash
Sprockell 0 says Even number
Sprockell 0 says 10
Sprockell 0 says 9
Sprockell 0 says 8
Sprockell 0 says 7
Sprockell 0 says 6
Sprockell 0 says 5
Sprockell 0 says 4
Sprockell 0 says 3
Sprockell 0 says 2
Sprockell 0 says 1
```

### 2. Peterson's Algorithm

The following code implements Peterson's algorithm which is a concurrent programming algorithm for mutual exclusion that allows two or more processes to share a single-use resource without conflict, using only shared memory for communication.

```c
// 2 shMem slots
global bool flag0 = false;
global bool flag1 = false;

// 3rd shMem slot - turn can be 0 or 1
global int turn;

// 4th shMem slot - critical variable
global int crit = 10;

// 5th shMem slot - thread 1
thread {

    int i;
    while (i < 10) {

        flag0 = true;
        turn = 1;
        while ( flag1 && turn == 1 ) {
            // busy wait
        }
        // critical section
        crit = crit + 1;
        print(crit);

        flag0 = false;
        i = i + 1;
    }
}

// 6th shMem slot - thread 2
thread {

    int i;
    while (i < 10) {

        flag1 = true;
        turn = 0;

        while ( flag0 && turn == 0 ) {
            // busy wait
        }
        // critical section
        crit = crit - 2;
        print(crit);

        flag1 = false;
        i = i + 1;
    }
}
// note: thread 1 is declared earlier, thus it will most likely always start before thread 2 
```

The file containing this algorithm is located under `test/programs/peterson.txt`. To run this code use the following command:

```bash
stack run -- "./test/programs/peterson.txt"
```

Expected output:

```bash
Sprockell 1 says 11
Sprockell 2 says 9
Sprockell 1 says 10
Sprockell 2 says 8
Sprockell 1 says 9
Sprockell 2 says 7
Sprockell 1 says 8
Sprockell 2 says 6
Sprockell 1 says 7
Sprockell 2 says 5
Sprockell 1 says 6
Sprockell 2 says 4
Sprockell 1 says 5
Sprockell 2 says 3
Sprockell 1 says 4
Sprockell 2 says 2
Sprockell 1 says 3
Sprockell 2 says 1
Sprockell 1 says 2
Sprockell 2 says 0
```

### 3. Banking System

The following code implements a banking system that runs 4 threads performing parallel transactions. 

```c
// 2 shMem slots
global int balance1 = 1000;
global int balance2 = 1000;

// 3th shMem slot - one common lock for performing transactions
Lock opLock;

// 4th shMem slot - lock used as a barrier / join for threads
Lock barrier;

// lock barrier
// NOTE: THIS IS NOT A REENTRANT LOCK!
// at the end, main thread will try to lock barrier again
// but will have to wait for it to be unlocked by a thread
barrier.lock;

// display of nested threads
// it is also possible to create two threads in 1 thread
// ex: thread { thread{} thread{} }

// 5th shMem slot - thread 1 - balance1
thread {
    
    // 6th shMem slot - thread 2 - balance2
    thread {
        
        // 7th shMem slot - thread 3 - balance1
        thread {

            //8th shMem slot - thread 4 - balance2
            thread {

                // thread 4 - balance2
                int i;
                while (i < 100) {
                    int toBal = i / (100 / 2);
                    int amount1 = ((i / 5) + 1) * 5;
                    int amount2 = amount1 / 3;

                    // balance1 += amount1
                    if (toBal == 0) {
                        opLock.lock;

                        balance2 = balance2 - amount1;
                        balance1 = balance1 + amount1;

                        print(balance1);
                        print(balance2);

                        opLock.unlock;

                    // balance1 -= amount2
                    } else {
                        opLock.lock;

                        balance2 = balance2 + amount2;
                        balance1 = balance1 - amount2;

                        print(balance1);
                        print(balance2);

                        opLock.unlock;
                    }
                    i = i + 1;
                }
            }

            // thread 3 - balance1
            int i;
            while (i < 100) {
                int toBal = i / (100 / 2);
                int amount1 = ((i / 5) + 1) * 5;
                int amount2 = amount1 / 4;

                // balance2 += amount2
                if (toBal == 0) {
                    opLock.lock;

                    balance1 = balance1 - amount1;
                    balance2 = balance2 + amount1;

                    print(balance1);
                    print(balance2);

                    opLock.unlock;

                // balance2 -= amount1
                } else {
                    opLock.lock;

                    balance1 = balance1 + amount2;
                    balance2 = balance2 - amount2;

                    print(balance1);
                    print(balance2);

                    opLock.unlock;
                }
                i = i + 1;
            }
        }

        // thread 2 - balance2
        int i;
        while (i < 100) {
            int toBal = i / (100 / 2);
            int amount1 = ((i / 5) + 1) * 10;
            int amount2 = amount1 / 3;

            // balance1 += amount2
            if (toBal == 0) {
                opLock.lock;

                balance2 = balance2 - amount2;
                balance1 = balance1 + amount2;

                print(balance1);
                print(balance2);

                opLock.unlock;

            // balance1 -= amount1
            } else {
                opLock.lock;

                balance2 = balance2 + amount1;
                balance1 = balance1 - amount1;

                print(balance1);
                print(balance2);

                opLock.unlock;
            }
            i = i + 1;
        }
    }

    // thread 1 - balance1
    int i;
    while (i < 100) {
        int toBal = i / (100 / 2);
        int amount1 = ((i / 5) + 1) * 10;
        int amount2 = amount1 / 2;

        // balance2 += amount1
        if (toBal == 0) {
            opLock.lock;

            balance1 = balance1 - amount1;
            balance2 = balance2 + amount1;

            print(balance1);
            print(balance2);

            opLock.unlock;

        // balance2 -= amount2
        } else {
            opLock.lock;

            balance1 = balance1 + amount2;
            balance2 = balance2 - amount2;

            print(balance1);
            print(balance2);

            opLock.unlock;
        }
        i = i + 1;
    }

    // unlock barrier
    barrier.unlock;
}

// print("This text will be printed in parallel with other threads!");

// main will wait here until barrier will be unlocked
// because barrier is not a reentrant lock
barrier.lock;

print("Final balances:");
print(balance1);    // -5050
print(balance2);    // 7050
```

The file containing this algorithm is located under `test/programs/banking.txt`. To run this code use the following command:

```bash
stack run -- "./test/programs/banking.txt"
```

Expected final lines of the output:

```bash
Sprockell 0 says Final balances:
Sprockell 0 says -5050
Sprockell 0 says 7050
```

## Known Limitations

- **Arrays and Strings:** Though the front-end parser allows their declaration, the back-end does not support them and will raise an error.

- **Memory Limitations:** Due to the SPROCKELL architecture’s limited shared memory, some concurrency programs might run into memory issues.