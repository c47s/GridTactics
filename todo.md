# Todo

*   [x] Add actor-switching to facilitate multiplayer

*   [x] Break UI into smaller functions

*   [x] Move general, reusable utility functions into a separate file

    Functions like stateToIO and doState

*   [x] Add multiplayer between terminals on same machine\
    Simply connect to localhost

*   [x] Add multiplayer over network

    *   [x] Make API

    *   [x] Make server

    *   [x] Make client

    *   [ ] Make it all work...

        *   [x] ... over localhost

        *   [ ] ... over LAN

        *   [ ] ... over the internet

*   [x] Delete Main\
    Pull out any useful code first (there probably is none since Client was originally a copy of Main)

*   [x] Replace `show`-based UI components with specialized widgets

*   [x] Modify client to reject empty names

*   [x] Auto-generate keybind hints in UI and keybinds themselves from a common source

*   [x] Write how2play

*   [ ] Create documentation

    *   [x] Client

    *   [ ] Server

    *   [ ] API

    *   [ ] GridTactics

    *   [ ] Mechanics

    *   [ ] SeqWorld

    *   [ ] Util

    *   [ ] WebInstances

*   [x] Refactor Loot as a Map from Resources to Ints

*   [ ] Make Client error-tolerant

*   [x] Allow Client to manage multiple actors

*   [ ] Add logging to server

*   [ ] Store a history of world states

*   [ ] CHALLENGE: Can I rewrite modifyM using mmorph functions?

*   [ ] Improve UI

    *   [x] List o' Actions and corresponding keybinds on left side of map

        *   [x] Make clickable

    *   [x] Choose any player in player change screen

    *   [ ] Use mouse to choose direction

    *   [x] Clickable buttons for other UI interactions

        *   [ ] ~~Submitting Action~~

        *   [x] Changing Actor

    *   [ ] Make action queue editable

        *   [x] Allow deleting last queue item

    *   [ ] Add Forms to UI\
        Will probably require adding lens stuff
