
# Elm Architecture Talk
Elm is a purely functional programming language for the frontend. It compiles to JavaScript and has an architecture that allows it to stay pure in an event-driven environment such as web browsers. By architecture I mean a way to structure how data flows through the program, how it gets changed, and how side effects happen.

It turns out that this architecture works very well with Haskell programs too. It can help us with writing programs that are quite explicit about what kind of things happen in them: what events it responds to, how and where data is changed, and what is all of the information being used to perform each state transition. 

This architecture is based on a very simple yet powerful idea. What if we described all possible changes to our program in one single pure function. A pure function in a strongly typed language is the easiest type of function to understand. It gets some information as input, it has only that to work with, and in all of its branches it must return something that matches what is specified in the type signature. Piggies go in, sausages come out, no magic.

This simplicityhowever, is key to the appeal of the Elm architecture. As we will see it is just a simple state machine, nothing that couldn't be implemented with Monads. But its shape forces us to be explicit about key areas of our program. I will argue that this being explicit leads to building programs that are easier to understand and encourages more thoughtful designs. 

Consequences
- Single state
- Have to think about the entire lifetime of our program beforehand -> leads to better design
- Single type to represent all state changes

- Simple
- Easy to understand
	- Easier to collaborate
	- 
- Ideal for building UIs
- Is very explicit about what kind of things happen
- You can see at a glance in the Msg type all possible ways in which the program could change.
