# Actions

| Action      | Description |
| :---        | :---        |
| Move        | Move one space. Pick up any resources you step on for free. |
| Jump        | Hurl yourself at +1 range, i.e. move range - health + 1 squares with billiard collisions. |
| Shoot       | Shoot scrap along a straight line, up to your range. Deal 1 damage to whatever you hit. |
| Blast       | Shoot scrap along a straight line, up to your range. Deal 1 damage to whatever you hit, and to the eight adjacent squares. |
| Throw       | Toss scrap and/or juice along a straight line, up to your range. If you hit something solid, it will catch the resources. Otherwise, they will drop on the ground. |
| Build       | For the price of X scrap, toss a wall with X health out to a maximum distance of range - X + 1. |
| Hurl        | Toss an adjacent entity up to your range minus its health. On collision, the hit entity will be hurled with any remaining range. | 
| Loot        | Extract resources from something with zero health. A dead pawn can only be looted once, yielding half its resources. |
| Repair      | Increase an adjacent entity's health by 1. |
| Repair Self | For a greater juice cost, increase your own health by 1. |
| Scrap Self  | Decrease your health by 1, but generate 3 juice. As with all damage, the lost health also becomes scrap. |
| Eat Scrap   | Convert 2 scrap into 1 juice. |
| +Range      | Increase this pawn's range by 1. |
| +Vision     | Increase this pawn's vision radius by 1. |
| Wait        | Do nothing. Useful for multi-pawn choreography. |
