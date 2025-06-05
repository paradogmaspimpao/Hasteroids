# Hasteroids
A Classic Asteroids game, written in Haskell with OpenGL libraries, using GLFW for window management.

### Dependencies:

To build and run Hasteroids, you will need the following:

-   **GHC (Glasgow Haskell Compiler):** The primary Haskell compiler. Most recent versions should work (e.g., GHC 8.6+).
-   **Cabal:** The Haskell build tool. Usually installed as `cabal-install`.
-   **GLFW (C Library):** Used for creating windows, contexts, and handling input.
    -   **macOS (Homebrew):** `brew install glfw`
    -   **Debian/Ubuntu:** `sudo apt-get install libglfw3-dev`
    -   **Fedora:** `sudo dnf install glfw-devel`
    -   Other systems: Install via your system's package manager. Ensure you install the development package if available (often ending in `-dev` or `-devel`).
-   **OpenGL Drivers:** Necessary for graphics rendering. These are typically provided by your graphics card vendor and operating system.

### Building and Running:

1.  **Navigate to the project directory:**
    If you've just cloned the repository, `cd Hasteroids`.

2.  **Build the project:**
    Use Cabal to build the game:
    ```bash
    cabal build
    ```
    This command will download and build any Haskell dependencies listed in the `Hasteroids.cabal` file.

3.  **Run the game:**
    After a successful build, run the executable:
    ```bash
    cabal run hasteroids
    ```

    Alternatively, you can find the executable in a path similar to `dist-newstyle/build/<arch-os>/ghc-<version>/hasteroids-<version>/x/hasteroids/build/hasteroids/hasteroids` and run it directly.

### Contributing

 - Grab an issue: Assign Yourself, and move the issue in the project's Kanban
 
 - Create a branch: Do all your work on a separate branch, named accordingly.
 
 - The last commit: Should start with "Closes #XYZ", where XYZ = Issue Number.
 
 - Open a pull-request and wait for review.

##### Many thanks to user @shangaslammi for his awesome guide.
