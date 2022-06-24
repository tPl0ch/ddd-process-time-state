# Functional Aggregate Design

## Processes, Temporality & Automata Theory

This repository contains the example code to [my talk][1] I have given at [Domain-Driven Design Europe '22][2].
I recommend giving the slides a look before diving into the repository.

It shows how one can use the mathematical definition of a Transducer, decompose it to its individual parts and create a
functional DDD Aggregate implementation.

### Requirements

* JVM 11+
* SBT 1.6+

### How to get started

#### Run the examples in the example folders

I recommend firing up the debugger and making breakpoints wherever you want to get an idea of how the examples work.

1. [Simple state store with a regular state machine](src/main/scala/examples/RegistrationStateOnly.scala)
2. [State store & Event publishing through Transactional Outbox](src/main/scala/examples/StateStoreAndTransactionalOutbox.scala)
3. [Event-Sourcing](src/main/scala/examples/EventSourcedRegistration.scala)
4. [Read-Model projection](src/main/scala/examples/ReadModelProjection.scala)
5. [Various Error scenarios](src/main/scala/examples/VariousErrorScenarios.scala)

The main point here is that in order to implement all these different use-cases, you don't have to change the model at all.
We can just use composition to create use-cases of any complexity.

#### Have a look at the domain

1. [Domain model](src/main/scala/domain/registration/Model.scala)
2. [Behaviors](src/main/scala/domain/registration/Behaviors.scala)
3. [Events](src/main/scala/domain/registration/Events.scala)
4. [Required implicit instances](src/main/scala/domain/registration/Givens.scala)
5. [Read model](src/main/scala/domain/registration/ReadModel.scala)
6. [Type aliases](src/main/scala/domain/registration/Types.scala)
7. [Generation of State Machine & Transducer based Aggregates](src/main/scala/domain/registration/Machines.scala)

#### Check out the implementations

1. [Lifecycle](src/main/scala/Lifecycle.scala)
2. [Invariants](src/main/scala/Invariants.scala)
3. [Transitions](src/main/scala/Transitions.scala)
4. [Aggregate initialization](src/main/scala/Aggregate.scala)
5. [Event-Sourcing](src/main/scala/EventSourcing.scala)

### Feedback

Feedback is very welcome! Let me know if you find something that can be improved or is hard to understand.

[1]: https://2022.dddeurope.com/program/functional-aggregate-design:-processes-temporality-and-automata-theory
[2]: https://2022.dddeurope.com/
