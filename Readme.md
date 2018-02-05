# Rubik-cube mode 
## What is it about

This is a project born out of a) desire to solve the puzzle by myself,
that is with computer assistance b) slight curiousity about the method
of mathemathical modelling of a rubic's cube.

At the moment, it allows to "play around" with a model of rubik's
cube. In the future, certain forms of automatic solving will be
available.

## License

This is Beerware, although a shavarma is a suitable substitute for
beer.

## Usage

Keymap for manipulation of the cube is based on notation of the
"language of rotations", with some caveats. For clockwise rotations,
same symbol is used, i.e.:

>    T  -> t

For obvious reasons, counter-clockwise and double rotations badly
translate into key sequences directly, so there are keymaps on 2 and i
for double and counterclockwise rotations respectively, i.e.:

>    T' -> i t

>    T2 -> 2 t

As a reminder, standard rotations are Top, Bottom, Left, Right, Front,
Back. Middles are not supported/

For convenience, there are undo and redo queues of commands; undoing
is invoked with M-u, and redoing with M-r.

To return to the initial state, standatd command g is used.

To have something to solve, use command M-s to shuffle the cube
randomly. If result is too hard to solve, one can reverse the shuffle
after displaying its undo queue with M-s.
