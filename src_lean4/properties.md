Property Proofs To Target

1) Craftax chest loot invariants (always)
   - rollChestLoot: potion count is 0 or 1 (never >1).
   - rollChestLoot: arrows count is either 0 or in [2,6].
   - rollChestLoot: gem count is 0 or 1, and if gem count is 1 then exactly one of sapphire/ruby is 1.
   - rollChestLoot: coal count is 0 or in [1,2]; iron count is 0 or in [1,2]; diamond is 0 or 1.
   - rollChestLoot respects drop chances at extremes:
     - if potionDropChance = 0, potion count = 0.
     - if arrowDropChance = 0, arrows = 0.
     - if gemDropChance = 0, gem count = 0.

2) Craftax XP/leveling invariants (always)
   - grantXp never decreases xp, level, or statPoints.
   - grantXp increments level only when xp >= 10*(nextLevel).
   - statPoints increments exactly once per level-up.
   - achievement gainXp increments iff xpEnabled and achievementsEnabled.
   - reachLevel increments iff achievementsEnabled and a level-up occurs.

3) Damage reduction invariants (always)
   - applyPlayerDamageWithReduction never increases health.
   - If baseDamage > 0 and healthEnabled, then applied damage >= 1.
   - reduction is clamped to [0.0, 0.9].
   - Sleeping multiplier applies before reduction (structural property of computation).

4) Arrow interaction invariants (always)
   - Arrow removed if it hits player, an object, or leaves bounds.
   - Arrow removed if it hits Table/Furnace; material becomes Path.
   - Arrow passes through Water and Lava.
   - Arrow never moves into a non-walkable, non-water, non-lava tile.
   - PlayerArrow kill grants XP (2 for zombie/skeleton, 3 for craftax mob).

5) Mob AI bounds (always)
   - Movement deltas are in {(-1,0),(1,0),(0,-1),(0,1)}.
   - Mobs never move out of bounds (if starting position is in bounds).
   - Mobs never move onto a blocking object when move is applied.
   - Skeleton reload never negative; resetReload sets to 4.
   - Zombie cooldown in [0,5].

6) Spawn/despawn invariants (always)
   - Despawn only for dist > 30.
   - Spawn only if tile is walkable and no object occupies it.
   - Craftax passive spawns respect terrain: Snail on Grass, Bat on Path.
   - Hostile craftax spawns only at night (daylight < 0.5).

7) Terrain interaction invariants (always)
   - Mining never yields items when pickaxe tier too low.
   - Mining converts material to Path/Grass as specified by Material.minedReplacement.
   - Drinking water resets thirstCounter to 0.
   - Grass sapling drop is ≤ 1 per action.

8) Plant invariants (always)
   - Plant.grow never decreases grown.
   - Plant.grown <= 300; once ripe, it stays ripe.
   - Plants only take damage from adjacent hostile mobs or cows.

9) RNG invariants (always)
   - ChaCha8.nextNat upper=0 returns 0.
   - nextNat upper=n produces value < n.
   - nextFloat32 outputs in [0.0, 1.0).

10) Serialization invariants (roundtrip targets)
   - Save/load roundtrip preserves world grid sizes.
   - Save/load roundtrip preserves player position and inventory counts.

11) Inventory invariants (always)
   - All inventory slots are in [0, maxValue] after any add* operation.
   - capAdd saturates at maxValue and never decreases a slot.
   - subOrZero never yields negative (always >= 0).
   - bestPickaxeTier/bestSwordTier match item presence.
   - attackDamage is consistent with bestSwordTier mapping.
   - armorReduction = 0.1 * (#armor pieces present) and in [0.0, 0.4].
   - addXp saturates at UInt32 max.

12) Crafting invariants (always)
   - Craft action only succeeds if required adjacent station is present (table/furnace).
   - Craft action only consumes resources when requirements met; otherwise inventory unchanged.
   - Craft action increases achievements only when craft succeeds and achievementsEnabled.
   - Crafting never produces items beyond cap (inventory cap preserved).

13) Action invariants (always)
   - Action.Move* updates facing even if move is blocked.
   - Action.Do only affects facing tile (object or terrain).
   - Action.Sleep sets sleeping true; wake-up only via action, damage, or energy full.
   - ShootArrow decreases arrows by 1 iff bow and arrows available.
   - DrinkPotion consumes exactly one potion of the specified kind iff available.

14) Player life-stat invariants (always)
   - updateLifeStats never increases counters beyond defined step rules.
   - Hunger/thirst counters reset only when a unit of food/drink is consumed.
   - Health only regenerates when not depleted (food/drink/energy) and healthEnabled.
   - When healthEnabled=false, health is always maxValue.
   - Sleeping reduces hunger/thirst increment rate and increases health regen rate.

15) Daylight/time invariants (always)
   - daylight ∈ [0.0, 1.0].
   - If dayNightCycle=false, daylight remains constant.
   - With dayNightCycle=true and period>0, daylight is deterministic function of step.

16) Worldgen invariants (always)
   - Generated world dimensions match config worldSize.
   - Spawn area is walkable (player spawn never inside obstacle).
   - Initial player object exists and is unique.
   - Generated materials are valid enum values; grid fully initialized.
   - Worldgen deterministic for fixed seed/config.

17) Object/index invariants (always)
   - Object IDs are unique (no duplicates).
   - Player object ID equals playerId in OracleState.
   - GameObject.position matches stored coords; moveObject updates only that object.
   - OracleState.getObjectIdAt? returns at most one id (by uniqueness).

18) Movement/collision invariants (always)
   - canMoveTo implies inBounds and walkable.
   - applyMovement never moves into blocking object.
   - Movement only changes position by one tile in cardinal directions.

19) Damage source invariants (always)
   - lastDamageSource updated on all health decreases.
   - Damage sources map correctly for zombie, skeleton, craftax melee/ranged/magic, lava, starvation, thirst, exhaustion.

20) Craftax mob invariants (always)
   - craftaxStats fields are consistent with kind definitions (health/damage/range/cooldown).
   - isHostile/isPassive classification matches kinds.
   - Ranged attacks only produce projectile kinds allowed by stats.

21) Plant placement/interaction invariants (always)
   - Plant can be placed only on grass with no object.
   - Plant harvest only when ripe; yields food and achievement increment.

22) Arrow invariants (always)
   - Arrow damage never negative; PlayerArrow uses bestSwordTier+2.
   - Arrow removal on hit is idempotent (target removal + arrow removal doesn’t corrupt object list).

23) RNG determinism invariants (always)
   - ChaCha8 seeded from seedFromU64 produces deterministic sequence.
   - RNG state updates are functional (no hidden mutation outside explicit state).

24) Config invariants (always)
   - Craftax features disabled => no craftax items/mobs/xp changes (as per flags).
   - healthEnabled=false => death condition disabled in stepDone.
   - maxSteps None => done only via death (if healthEnabled).

25) Rendering/obs invariants (always)
   - Observation.playerPos matches current player position.
   - Observation.step equals OracleState.step.

26) Save/load invariants (roundtrip targets)
   - Save/load preserves config flags and craftax sub-configs.
   - Save/load preserves object counts and object kinds.
   - Save/load preserves daylight and step/episode counters.
