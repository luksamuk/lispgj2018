# ODS: Orbit Defense Strikeforce
My entry on Lisp Game Jam 2018.

This game is written in Common Lisp, built using SBCL, Emacs+SLIME, and Quicklisp.

You'll also need SDL2 and SDL2_Image in order to build and execute the game.

This entry makes extensive use of my `game-sketch` system, which I built to help me create this game.
You'll find it, along with install instructions, [in this repository](https://github.com/luksamuk/game-sketch).

This project is distributed under the MIT License.

# Compiling

This assumes that you have Quicklisp installed on your home folder (`~/quicklisp`), and it also assumes that
you have cloned or symlinked the `game-sketch` project on your Quicklisp's `local-projects` directory.
Also, you'll need SDL2 and SDL2_Image runtimes installed on your computer, as per `cl-sdl2` and `cl-sdl2-image`
requirements.

Then, just go right ahead on the `src` directory and type `make`.

```bash
$ cd /path/to/ods
$ cd src
$ make
```

This will generate a `cl-ods.your-architecture` binary, which is an SBCL dumped core.

Compiling on Windows assumes you have a very particular setup of Wine just to compile Lisp games for
SBCL, so perharps you should execute it manually if you don't want to grab the precompiled binary releases.

# Loading manually

Assuming you have the dependencies and Quicklisp configured on your PATH, and that you also have `game-sketch`
installed in your Quicklisp's `local-projects` directory, you can just tell SBCL to load the `sketch.lisp`
file:

```bash
$ sbcl --load sketch.lisp
```

...and then run the game from REPL:

```lisp
(run-game)
```

# License

This project is distributed under the MIT License. See `LICENSE` or the source code for details.

