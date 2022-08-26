# How to Play

## Starting a Game

To create a world, run the Server program and choose a port. The default `42069` should be fine. If you don't know what options to choose, just keep hitting enter until you see `Starting Server...`

Next, each player should run the Client program on the same machine as the Server. Accept the default hostname, `localhost`, and enter the same port the Server is using. If people are playing on different machines, they should log in to the same machine via `ssh` or a similar service, then run Client.

Once you're comfortably situated and no-one can see your screen, press `y` to enter your tactical control panel.

## Playing the Game

All players plan their turns simultaneously, and actions only happen once everyone is done planning.

Everyone's actions happen in order, i.e. everyone's first actions happen before everyone's second actions. However, beyond this, the order is random, i.e. one player's first action could happen before or after another player's first action.

First, select an action. You can either click it in the menu to the left, or press its hotkey, which is also listed in the menu.
Once the action is selected, you can rotate its direction and otherwise modify it, if applicable.
Then, hit enter to add it to your plan.
You can add any number of actions to your plan, even if you don't have enough action points yet. You could always get more actions during the round by shooting yourself, getting actions thrown at you, or picking up actions from the ground.

Once you're satisfied with your plan, press d or click End Turn to signal that you're done. Once all players end their turn, the round is evaluated.

## Tips

### Actions

#### Moving
You can't move onto walls unless they have 0 health.

You can never move onto players.

If you move onto a square, you pick up its contents.

#### Shooting
Each shot deducts one heath point from the recipient and gives them one health chunk.

Shooting yourself has a negative cost! This means you gain an action point by doing it. 

#### Throwing
Throwing and shooting have the same range — upgrading range upgrades both.

If thrown items hit a player, they'll be added directly to that player's inventory. If they hit a wall, they fall on the ground in front of the wall. Finally, if they don't hit anything, they fall on the ground at the edge of your range.

Every health chunk you throw decreases your maximum health by 1. This is because maximum health is really just health chunks + health points.

#### Grabbing
You can only grab adjacent squares.

#### Healing
You can only heal adjacent squares.

Get next to an ally — you can heal each other more efficiently than you can heal yourselves.

#### Upgrading
Upgrading vision increases your vision radius by 1, and upgrading range increases your throwing and shooting range by 1.

#### Waiting
Waiting is helpful when coorinating with teammates. For instance, if someone else plans to move within range and then toss you some action points, you'll need to wait until after they throw to be sure you've recieved those points. Likewise, if someone else plans to heal you from 0 health, you'll want to wait until after they do that.

However, if you have the action points to spare, doing things while another player also acts is often more productive — wait only when necessary.

### Loot
Loot looks like a square with 0 health — it's either a dead player, a dead wall, or some stuff someone threw. The number after the slash is the number of health chunks the loot contains. The action points it holds can only be determined by looting it!

Depending on the settings of your server, walls might contain action points. Ask your host about their parameters!

### Health
Maximum health is a social construct! The number after the slash on a square (i.e. the 3 in 1/3) is just health points plus health chunks in inventory. Since damage takes away one point and gives one chunk, this sum acts just like maximum health. This is why throwing health chunks decreases your "maximum health."

### Tactics
Players' positions become more uncertain as the round progresses, since they've had more time to move. You could shoot early to mitigate this, but you may also want to move early in order to dodge other players' volleys.

Aim your shots so that they pass through multiple squares your target could be in. If you have the action points, consider shooting in multiple directions or even moving and then shooting from a slightly different angle.

If you try shooting yourself while in a firefight, your opponent will probably kill you before you use your extra action points. Instead, have someone behind you shoot themselves and throw you the points.

Clustering allies is very helpful: you can throw action points to each other, and heal each other if adjacent. Players in front can thus recieve healing and points from players behind, sustaining them in a firefight.