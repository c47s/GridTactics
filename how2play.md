# How to Play

## Installing GridTactics

Just clone the repository, install [stack](https://docs.haskellstack.org/en/stable/), then `cd GridTactics` into the base directory and use `stack run Server` or `stack run Client` to automatically compile and run the desired program. Might take a while the first time, what with all the dependencies.

## Starting a Game

To create a world, run the Server and choose a port. The default `42069` should be fine. If you don't know what options to choose, just keep hitting enter until you see `Starting Server...`

Next, each player should run the Client on the same machine as the Server. I'd recommend that each person play on a different machine and then log in to the server machine via `ssh` or similar. Accept the default hostname, `localhost`, and enter the same port the Server is using.

Once you're comfortably situated and no-one can see your screen, press `y` to enter your tactical control panel.

## What am I looking at?!

### Pawns

```
       Living Pawn
         ┌─────┐
 Name -> │Zau  │
 Init -> │#3 ❤︎2│ <- Health
         └─────┘
```
```
        Dead Pawn
         ┌─────┐
 Init -> │#2 ❤︎0│ <- Health
Juice -> │3J 4S│ <- Scrap
         └─────┘
```

Your selected pawn is highlighted blue, friendly pawns are green, and others are red.
You can click a green pawn to switch to it.

### Objects

```
          Wall
         ┌─────┐
         │   ❤︎2│ <- Health
Juice -> │1J 1S│ <- Scrap
         └─────┘
```
```
    Loot (Dead Wall)
         ┌─────┐
Juice -> │2J 2S│ <- Scrap
         │     │
         └─────┘
```
```
           Fog
         ┌─────┐
         │  ?  │
         │     │
         └─────┘
```

Walls are highlighted black. Fog is highlighted white.

## Playing the Game

All players plan their turns simultaneously, and actions only happen once everyone is done planning.

When evaluating a round, we cycle through pawns, letting them attempt one action per turn. Their position in this cycle is displayed on their square -- it looks like e.g. `#1` or `#3`.

First, select an action. You can either click it in the menu to the left, or press its hotkey, also listed in the menu.
Once the action is selected, you can rotate its direction and otherwise modify it, if applicable. The keys to do so will display beneath the action menu to the left.
Then, hit enter to add it to your plan. Delete to remove the last action you added.
You can add any number of actions to your plan, even if you don't have enough resources to perform them yet. You could always get more during the round by shooting yourself, getting them thrown at you, or picking them up from the ground.

Once you're satisfied with your plan, press d or click End Turn to signal that you're done. Once all pawns end their turn, the round is evaluated.

## Tips

### Actions

#### Moving
You can't move onto walls unless they have 0 Health.

You can never move onto pawns.

If you move onto a square, you pick up its contents.

#### Shooting
One damage, at range. The timeless classic.

### Blasting
Same as shooting, but damages each square adjacent to the target, too.
Otherwise, if nothing is in the way, bursts at the end of your range.
Drops some scrap at the impact point.

#### Throwing
Throwing and shooting have the same range — upgrading range upgrades both.

If thrown items hit a pawn, they'll be added directly to that pawn's inventory. If they hit a wall, they fall on the ground in front of the wall. Finally, if they don't hit anything, they fall on the ground at the edge of your range.

#### Looting
You can only grab loot from adjacent squares, and only when they have 0 health.
Stepping on loot grabs it for free, but you can't move onto pawns.
Dead pawns can only be looted once, yielding half their contents.

#### Repairing
You can only repair adjacent squares.
There's no health limit so long as you have the juice & scrap.

Get next to an ally — you can repair each other more efficiently than you can repair yourselves.

#### Building
Create a wall with 1 health per scrap spent, and toss it up to your range, or 1 fewer space per additional health over 1.

#### Scrap Juicing
You can scrap yourself, taking 1 damage (thereby generating 1 scrap), and gaining 1 juice as well.
You can also eat scrap, converting it to juice; 2 scrap yield only 1 juice.

#### Upgrading
Upgrading vision increases your vision radius by 1, and upgrading range increases your throwing and shooting range by 1. The maximum depends on the size of your game world.

If your range is large enough, you could shoot around the world and hit yourself in the back...

#### Waiting
Waiting is helpful when coorinating with teammates. For instance, if someone else plans to move within range and then toss you some juice, you'll need to wait until after they throw to be sure you've recieved it. Likewise, if someone else plans to repair you from 0 health, you'll want to wait until after they do that.

### Juice & Scrap
Each pawn recieves one juice per round, but scrap is only generated when entities (walls, pawns) take damage. The sum of health + scrap in the world can only decrease (via blasting, scrap eating).

### Tactics
Pawns' positions become more uncertain as the round progresses, since they've had more time to move. You could shoot early to mitigate this, but you may also want to move early in order to dodge other players' volleys.

Aim your shots so that they pass through multiple squares your target could be in. If you have the action points, consider shooting in multiple directions or even moving and then shooting from a slightly different angle. Or just spray blasts. If you have the scrap & juice.

If you try scrapping yourself while in a firefight, your opponent will probably kill you before you use your extra juice. Instead, scrap from cover and toss the spoils to the front line.

Clustering allies is very helpful: you can throw resources to each other, and repair each other if adjacent. Pawns in front can thus recieve support from pawns behind, sustaining them in a firefight.

You can use the combined vision in map view to locate targets with one pawn's view, then line up an unexpected shot from unseen pawns behind it.
