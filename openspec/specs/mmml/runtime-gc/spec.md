# mmml/runtime-gc Specification

## Purpose

Reclaim unreachable closures in the MMML runtime automatically and on demand, without moving objects, and define the root contract that generated code must honor.

## Requirements

### Requirement: Explicit collection entry point

The runtime SHALL expose a void, zero-argument `mmml_collect` entry point that generated assembly or tests may call at any time after `mmml_init`. Calling it SHALL reclaim every closure that is not reachable from the collector's roots.

#### Scenario: Collection reclaims dropped closures

- **WHEN** a program creates closures, drops all references to some of them, and calls `mmml_collect`
- **THEN** the runtime's allocation list no longer contains the dropped closures
- **AND** memory for those closures is released

#### Scenario: Collection before initialization

- **WHEN** `mmml_collect` is called before `mmml_init`
- **THEN** the runtime fails with the standard "mmml_init must be called first" error

### Requirement: Reachable closures survive

The collector SHALL never free a closure that is reachable from conservative roots on the stack or from the filled argument slots of another reachable closure. Unfilled argument slots SHALL be ignored.

#### Scenario: Chain of captures survives

- **WHEN** a live closure captures another closure in its filled arguments and a collection runs
- **THEN** both closures remain valid and `mmml_apply` succeeds on them afterwards

#### Scenario: Unfilled slots are not traced

- **WHEN** a live closure has filled and unfilled argument slots and a collection runs
- **THEN** the collection reads no unfilled slot and the closure remains fully usable

### Requirement: Objects do not move

Collection SHALL NOT relocate closures. A pointer to a live closure obtained before a collection SHALL remain a valid pointer to the same closure after it.

#### Scenario: Pointer identity across collection

- **WHEN** a program stores a closure pointer, triggers `mmml_collect`, and then applies the stored pointer
- **THEN** the application succeeds on the same closure

### Requirement: Automatic collection on allocation pressure

`mmml_make_closure` SHALL trigger a collection before allocating when heap usage since the previous collection exceeds a threshold. Creating a closure SHALL behave identically whether or not a collection ran first.

#### Scenario: Threshold triggers collection

- **WHEN** accumulated closure allocation exceeds the runtime's threshold and the program creates another closure
- **THEN** a collection runs as part of that `mmml_make_closure` call
- **AND** the returned closure is valid

### Requirement: Root contract for generated code

Generated assembly SHALL keep every live `mmml_value` in stack memory whenever it makes a runtime call that may collect; argument and caller-saved registers are not roots. Any live value stored on the stack SHALL survive a collection triggered by such a call.

#### Scenario: Live value in a frame slot survives

- **WHEN** generated code stores a live closure in a frame slot, then makes a runtime call whose callee triggers a collection
- **THEN** the slot's closure remains valid after the call returns

### Requirement: Full-application arguments stay reachable

The runtime SHALL keep the argument buffer used for a full `mmml_apply` reachable to the collector for the duration of the call, so captured values survive a collection triggered inside the callee.

#### Scenario: Callee triggers collection

- **WHEN** applying a closure whose body allocates enough to trigger a collection, with closures among the applied arguments
- **THEN** the callee runs with all its arguments intact
- **AND** the call completes normally

### Requirement: Stale pointers are rejected

After a collection frees a closure, applying a pointer to that freed closure SHALL fail with a clean runtime error instead of exhibiting undefined behavior, as long as the freed memory has not been reused for a new closure.

#### Scenario: Applying a collected closure

- **WHEN** a program keeps a raw pointer to a closure, makes the closure unreachable, collects, and applies the raw pointer
- **THEN** the runtime reports an invalid closure error

### Requirement: Collection is repeatable

Repeated collections SHALL be safe. A second collection immediately after the first SHALL free no live closure and SHALL return the runtime to the same state, apart from reused memory.

#### Scenario: Two collections in a row

- **WHEN** a program triggers `mmml_collect` twice without allocating in between
- **THEN** the second collection completes normally and all closures live after the first remain live
