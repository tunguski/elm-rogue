module Mod.Default exposing (ruleset)

{-| The default mod: the stock bestiary and hero, expressed entirely as data.

This module is the reference example of how content is authored. To make a mod you copy this file,
tweak the numbers (or add/remove `EnemyDef`s) and hand the resulting `Ruleset` to the engine — see
`Mod.Hard` for a difficulty mod that does exactly that. The bestiary is loosely modelled on Shattered
Pixel Dungeon's early floors (rats and gnolls up top, tougher crabs and skeletons deeper).
-}

import Rogue.Content exposing (ClassDef, EnemyDef, EquipSlot(..), HeroDef, ItemDef, ItemEffect(..), ItemKind(..), MonsterAbility(..), Ruleset)


ruleset : Ruleset
ruleset =
    { name = "Default"
    , hero = hero
    , classes =
        [ warrior
        , mage
        , rogue
        , huntress
        , duelist
        ]
    , enemies =
        [ rat
        , marsupialRat
        , gnollScout
        , gnollArcher
        , crab
        , skeleton
        , swarm
        , slime
        , thief
        , gnollBrute
        , gnollShaman
        , prisonGuard
        , caveBat
        , piranha
        , stoneGolem
        , necromancer
        , dwarfMonk
        , demon
        , succubus
        ]
    , bosses =
        [ gnollWarlord
        , spiderQueen
        , dwarfKing
        , yogDzewa
        ]
    , items =
        [ healingPotion
        , greaterHealingPotion
        , potionOfStrength
        , potionOfShielding
        , potionOfRegeneration
        , potionOfHaste
        , potionOfInvisibility
        , potionOfLevitation
        , potionOfMindVision
        , potionOfExperience
        , potionOfLiquidFlame
        , potionOfCausticGas
        , bomb
        , darts
        , javelin
        , shuriken
        , ankh
        , ration
        , torch
        , goldPile
        , dagger
        , shortSword
        , mace
        , rapier
        , vampiricDagger
        , blazingMace
        , grimGlaive
        , leatherArmour
        , mailArmour
        , plateArmour
        , thornedMail
        , scrollOfTeleport
        , scrollOfMagicMapping
        , scrollOfIdentify
        , scrollOfUpgrade
        , scrollOfRecharging
        , scrollOfTerror
        , scrollOfRemoveCurse
        , scrollOfLullaby
        , scrollOfRetribution
        , scrollOfTransmutation
        , scrollOfGrowth
        , scrollOfRage
        , scrollOfCorruption
        , scrollOfEnchantment
        , scrollOfMysticalEnergy
        , potionOfCleansing
        , wandMagicMissile
        , wandFirebolt
        , wandLightning
        , wandFrost
        , wandCorrosion
        , wandBlastWave
        , wandDisintegration
        , wandRegrowth
        , ringOfPower
        , ringOfProtection
        , ringOfMight
        , ringOfForce
        , ringOfEvasion
        , ringOfTenacity
        , cloakOfShadows
        , hornOfPlenty
        , chaliceOfBlood
        , timekeepersHourglass
        , driedRose
        , thievesArmband
        , etherealChains
        , unstableSpellbook
        , ironKey
        , amulet
        ]
    }


hero : HeroDef
hero =
    { maxHp = 20
    , damage = 4
    , defense = 1
    , glyph = "@"
    , color = "#ffe08a"
    , fovRadius = 7
    }



-- CLASSES ----------------------------------------------------------------------------------------


warrior : ClassDef
warrior =
    { id = "warrior"
    , name = "Warrior"
    , description = "Tough and well-armed. High HP and defense; opens with a short sword."
    , glyph = "@"
    , color = "#ffcf6a"
    , maxHp = 26
    , damage = 5
    , defense = 2
    , fovRadius = 7
    , startingWeapon = Just "short-sword"
    , startingArmour = Just "leather-armour"
    , startingItems = [ "potion-healing" ]
    }


mage : ClassDef
mage =
    { id = "mage"
    , name = "Mage"
    , description = "Fragile but hits hard and sees far. Opens with two healing potions."
    , glyph = "@"
    , color = "#7fb0ff"
    , maxHp = 16
    , damage = 6
    , defense = 0
    , fovRadius = 9
    , startingWeapon = Nothing
    , startingArmour = Nothing
    , startingItems = [ "potion-healing", "potion-healing" ]
    }


rogue : ClassDef
rogue =
    { id = "rogue"
    , name = "Rogue"
    , description = "Balanced and quick. Opens with a dagger and a potion of strength."
    , glyph = "@"
    , color = "#9be08a"
    , maxHp = 20
    , damage = 4
    , defense = 1
    , fovRadius = 8
    , startingWeapon = Just "dagger"
    , startingArmour = Nothing
    , startingItems = [ "potion-strength" ]
    }


huntress : ClassDef
huntress =
    { id = "huntress"
    , name = "Huntress"
    , description = "Keen-eyed and lethal at range. Sees farthest; opens with a sheaf of darts to throw."
    , glyph = "@"
    , color = "#6ad8a0"
    , maxHp = 18
    , damage = 4
    , defense = 1
    , fovRadius = 10
    , startingWeapon = Just "dagger"
    , startingArmour = Nothing
    , startingItems = [ "darts", "darts", "darts" ]
    }


duelist : ClassDef
duelist =
    { id = "duelist"
    , name = "Duelist"
    , description = "A nimble blademaster. Opens with a rapier and leather armour, trading bulk for finesse."
    , glyph = "@"
    , color = "#e0a0c0"
    , maxHp = 22
    , damage = 5
    , defense = 1
    , fovRadius = 8
    , startingWeapon = Just "rapier"
    , startingArmour = Just "leather-armour"
    , startingItems = [ "potion-healing" ]
    }


rat : EnemyDef
rat =
    { id = "rat"
    , name = "rat"
    , glyph = "r"
    , color = "#b08968"
    , maxHp = 8
    , damage = 2
    , defense = 0
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 3
    , spawnWeight = 10
    , xp = 1
    , drop = Nothing
    }


marsupialRat : EnemyDef
marsupialRat =
    { id = "marsupial-rat"
    , name = "marsupial rat"
    , glyph = "r"
    , color = "#caa472"
    , maxHp = 10
    , damage = 3
    , defense = 0
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 2
    , maxDepth = 4
    , spawnWeight = 6
    , xp = 2
    , drop = Nothing
    }


gnollScout : EnemyDef
gnollScout =
    { id = "gnoll-scout"
    , name = "gnoll scout"
    , glyph = "g"
    , color = "#7fae5a"
    , maxHp = 12
    , damage = 4
    , defense = 1
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 2
    , maxDepth = 5
    , spawnWeight = 8
    , xp = 3
    , drop = Nothing
    }


gnollArcher : EnemyDef
gnollArcher =
    { id = "gnoll-archer"
    , name = "gnoll archer"
    , glyph = "a"
    , color = "#a7c46a"
    , maxHp = 10
    , damage = 4
    , defense = 0
    , speed = 1
    , ranged = 4
    , ability = NoAbility
    , boss = False
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 5
    , xp = 4
    , drop = Nothing
    }


crab : EnemyDef
crab =
    { id = "crab"
    , name = "sewer crab"
    , glyph = "c"
    , color = "#d4756a"
    , maxHp = 15
    , damage = 5
    , defense = 2
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 6
    , xp = 4
    , drop = Nothing
    }


skeleton : EnemyDef
skeleton =
    { id = "skeleton"
    , name = "skeleton"
    , glyph = "s"
    , color = "#d9d2c2"
    , maxHp = 18
    , damage = 6
    , defense = 2
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 4
    , maxDepth = 12
    , spawnWeight = 5
    , xp = 6
    , drop = Nothing
    }


swarm : EnemyDef
swarm =
    { id = "swarm"
    , name = "swarm of flies"
    , glyph = "w"
    , color = "#9aa7ba"
    , maxHp = 6
    , damage = 2
    , defense = 0
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 3
    , maxDepth = 7
    , spawnWeight = 4
    , xp = 2
    , drop = Nothing
    }



slime : EnemyDef
slime =
    { id = "slime"
    , name = "green slime"
    , glyph = "j"
    , color = "#6fc06a"
    , maxHp = 14
    , damage = 3
    , defense = 1
    , speed = 1
    , ranged = 0
    , ability = Splits
    , boss = False
    , minDepth = 2
    , maxDepth = 6
    , spawnWeight = 4
    , xp = 3
    , drop = Nothing
    }


thief : EnemyDef
thief =
    { id = "thief"
    , name = "thief"
    , glyph = "t"
    , color = "#c9a0ff"
    , maxHp = 10
    , damage = 2
    , defense = 1
    , speed = 1
    , ranged = 0
    , ability = StealsGold 20
    , boss = False
    , minDepth = 2
    , maxDepth = 7
    , spawnWeight = 3
    , xp = 4
    , drop = Just "gold"
    }


gnollBrute : EnemyDef
gnollBrute =
    { id = "gnoll-brute"
    , name = "gnoll brute"
    , glyph = "G"
    , color = "#6f8f4a"
    , maxHp = 22
    , damage = 6
    , defense = 2
    , speed = 1
    , ranged = 0
    , ability = Regenerates 2
    , boss = False
    , minDepth = 4
    , maxDepth = 8
    , spawnWeight = 4
    , xp = 7
    , drop = Nothing
    }


gnollShaman : EnemyDef
gnollShaman =
    { id = "gnoll-shaman"
    , name = "gnoll shaman"
    , glyph = "h"
    , color = "#8ad6c0"
    , maxHp = 14
    , damage = 5
    , defense = 1
    , speed = 1
    , ranged = 5
    , ability = Heals 3
    , boss = False
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 4
    , xp = 5
    , drop = Just "scroll-magic-mapping"
    }


prisonGuard : EnemyDef
prisonGuard =
    { id = "prison-guard"
    , name = "prison guard"
    , glyph = "G"
    , color = "#b0a890"
    , maxHp = 24
    , damage = 6
    , defense = 4
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 4
    , xp = 6
    , drop = Nothing
    }


caveBat : EnemyDef
caveBat =
    { id = "cave-bat"
    , name = "cave bat"
    , glyph = "b"
    , color = "#a07fc0"
    , maxHp = 12
    , damage = 5
    , defense = 1
    , speed = 2
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 5
    , maxDepth = 8
    , spawnWeight = 5
    , xp = 5
    , drop = Nothing
    }


piranha : EnemyDef
piranha =
    { id = "piranha"
    , name = "piranha"
    , glyph = "p"
    , color = "#d4756a"
    , maxHp = 16
    , damage = 8
    , defense = 3
    , speed = 1
    , ranged = 0
    , ability = Aquatic
    , boss = False
    , minDepth = 5
    , maxDepth = 8
    , spawnWeight = 3
    , xp = 6
    , drop = Nothing
    }


stoneGolem : EnemyDef
stoneGolem =
    { id = "stone-golem"
    , name = "stone golem"
    , glyph = "8"
    , color = "#9a9aa0"
    , maxHp = 40
    , damage = 9
    , defense = 6
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 7
    , maxDepth = 12
    , spawnWeight = 4
    , xp = 10
    , drop = Just "mail-armour"
    }


necromancer : EnemyDef
necromancer =
    { id = "necromancer"
    , name = "necromancer"
    , glyph = "n"
    , color = "#b06ad8"
    , maxHp = 20
    , damage = 6
    , defense = 2
    , speed = 1
    , ranged = 4
    , ability = SummonsAllies
    , boss = False
    , minDepth = 6
    , maxDepth = 12
    , spawnWeight = 3
    , xp = 9
    , drop = Just "wand-magic-missile"
    }


dwarfMonk : EnemyDef
dwarfMonk =
    { id = "dwarf-monk"
    , name = "dwarf monk"
    , glyph = "m"
    , color = "#c0b090"
    , maxHp = 30
    , damage = 10
    , defense = 4
    , speed = 1
    , ranged = 0
    , ability = NoAbility
    , boss = False
    , minDepth = 9
    , maxDepth = 12
    , spawnWeight = 6
    , xp = 12
    , drop = Nothing
    }


demon : EnemyDef
demon =
    { id = "demon"
    , name = "ripper demon"
    , glyph = "D"
    , color = "#e0564b"
    , maxHp = 28
    , damage = 11
    , defense = 3
    , speed = 1
    , ranged = 0
    , ability = Burns
    , boss = False
    , minDepth = 11
    , maxDepth = 12
    , spawnWeight = 6
    , xp = 13
    , drop = Nothing
    }


succubus : EnemyDef
succubus =
    { id = "succubus"
    , name = "succubus"
    , glyph = "S"
    , color = "#c97fe0"
    , maxHp = 24
    , damage = 9
    , defense = 3
    , speed = 1
    , ranged = 5
    , ability = Heals 4
    , boss = False
    , minDepth = 10
    , maxDepth = 12
    , spawnWeight = 4
    , xp = 14
    , drop = Just "scroll-upgrade"
    }



-- BOSSES -----------------------------------------------------------------------------------------


gnollWarlord : EnemyDef
gnollWarlord =
    { id = "gnoll-warlord"
    , name = "Gnoll Warlord"
    , glyph = "W"
    , color = "#e0a83c"
    , maxHp = 60
    , damage = 9
    , defense = 3
    , speed = 1
    , ranged = 0
    , ability = Regenerates 2
    , boss = True
    , minDepth = 4
    , maxDepth = 4
    , spawnWeight = 0
    , xp = 30
    , drop = Just "mace"
    }


spiderQueen : EnemyDef
spiderQueen =
    { id = "spider-queen"
    , name = "Spider Queen"
    , glyph = "M"
    , color = "#b06ad8"
    , maxHp = 90
    , damage = 12
    , defense = 4
    , speed = 1
    , ranged = 3
    , ability = Splits
    , boss = True
    , minDepth = 7
    , maxDepth = 7
    , spawnWeight = 0
    , xp = 60
    , drop = Just "wand-firebolt"
    }


dwarfKing : EnemyDef
dwarfKing =
    { id = "dwarf-king"
    , name = "Dwarf King"
    , glyph = "K"
    , color = "#e0c24b"
    , maxHp = 140
    , damage = 14
    , defense = 6
    , speed = 1
    , ranged = 0
    , ability = SummonsAllies
    , boss = True
    , minDepth = 10
    , maxDepth = 10
    , spawnWeight = 0
    , xp = 90
    , drop = Just "plate-armour"
    }


yogDzewa : EnemyDef
yogDzewa =
    { id = "yog-dzewa"
    , name = "Yog-Dzewa"
    , glyph = "Y"
    , color = "#ff5a5a"
    , maxHp = 200
    , damage = 16
    , defense = 7
    , speed = 1
    , ranged = 4
    , ability = Burns
    , boss = True
    , minDepth = 12
    , maxDepth = 12
    , spawnWeight = 0
    , xp = 150
    , drop = Just "chalice-blood"
    }



-- ITEMS ------------------------------------------------------------------------------------------


healingPotion : ItemDef
healingPotion =
    { id = "potion-healing"
    , name = "potion of healing"
    , glyph = "!"
    , color = "#e0564b"
    , kind = Consumable (HealHp 10)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 16
    }


greaterHealingPotion : ItemDef
greaterHealingPotion =
    { id = "potion-greater-healing"
    , name = "potion of full healing"
    , glyph = "!"
    , color = "#ff8aa0"
    , kind = Consumable HealFull
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 6
    }


potionOfStrength : ItemDef
potionOfStrength =
    { id = "potion-strength"
    , name = "potion of strength"
    , glyph = "!"
    , color = "#caa472"
    , kind = Consumable (DamageBonus 1)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


potionOfShielding : ItemDef
potionOfShielding =
    { id = "potion-shielding"
    , name = "potion of shielding"
    , glyph = "!"
    , color = "#4f8bff"
    , kind = Consumable (MaxHpBonus 5)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 4
    }


potionOfRegeneration : ItemDef
potionOfRegeneration =
    { id = "potion-regeneration"
    , name = "potion of regeneration"
    , glyph = "!"
    , color = "#5dd47a"
    , kind = Consumable (Regenerate 2 8)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


potionOfHaste : ItemDef
potionOfHaste =
    { id = "potion-haste"
    , name = "potion of haste"
    , glyph = "!"
    , color = "#ffd34d"
    , kind = Consumable (HasteFor 20)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 4
    }


potionOfInvisibility : ItemDef
potionOfInvisibility =
    { id = "potion-invisibility"
    , name = "potion of invisibility"
    , glyph = "!"
    , color = "#bcd6ff"
    , kind = Consumable (Invisibility 14)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


potionOfLevitation : ItemDef
potionOfLevitation =
    { id = "potion-levitation"
    , name = "potion of levitation"
    , glyph = "!"
    , color = "#a7e0d6"
    , kind = Consumable (Levitation 16)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


potionOfMindVision : ItemDef
potionOfMindVision =
    { id = "potion-mind-vision"
    , name = "potion of mind vision"
    , glyph = "!"
    , color = "#d6a7e0"
    , kind = Consumable MindVision
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 3
    }


potionOfExperience : ItemDef
potionOfExperience =
    { id = "potion-experience"
    , name = "potion of experience"
    , glyph = "!"
    , color = "#e0d24b"
    , kind = Consumable (Experience 8)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


potionOfLiquidFlame : ItemDef
potionOfLiquidFlame =
    { id = "potion-liquid-flame"
    , name = "potion of liquid flame"
    , glyph = "!"
    , color = "#ff7a3c"
    , kind = Consumable (Incinerate 4)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


potionOfCausticGas : ItemDef
potionOfCausticGas =
    { id = "potion-caustic-gas"
    , name = "potion of caustic gas"
    , glyph = "!"
    , color = "#9bbf4a"
    , kind = Consumable (ToxicGas 3)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


bomb : ItemDef
bomb =
    { id = "bomb"
    , name = "bomb"
    , glyph = "ø"
    , color = "#9aa0a8"
    , kind = Consumable (Explode 12)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


darts : ItemDef
darts =
    { id = "darts"
    , name = "darts"
    , glyph = "↑"
    , color = "#b9c2d0"
    , kind = Consumable (ThrownHit 4)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


javelin : ItemDef
javelin =
    { id = "javelin"
    , name = "javelin"
    , glyph = "↑"
    , color = "#d6deea"
    , kind = Consumable (ThrownHit 9)
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 3
    }


shuriken : ItemDef
shuriken =
    { id = "shuriken"
    , name = "shuriken"
    , glyph = "↑"
    , color = "#c9a0ff"
    , kind = Consumable (ThrownHit 6)
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


ankh : ItemDef
ankh =
    { id = "ankh"
    , name = "ankh"
    , glyph = "Ω"
    , color = "#e0d24b"
    , kind = Consumable HealFull
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


ration : ItemDef
ration =
    { id = "ration"
    , name = "ration of food"
    , glyph = "%"
    , color = "#c98b3c"
    , kind = Consumable (Feed 250)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 6
    }


torch : ItemDef
torch =
    { id = "torch"
    , name = "torch"
    , glyph = "!"
    , color = "#ffb347"
    , kind = Consumable (LightFor 4 60)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


goldPile : ItemDef
goldPile =
    { id = "gold"
    , name = "gold"
    , glyph = "$"
    , color = "#d8b24c"
    , kind = Consumable (Gold 15)
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 12
    }



-- WEAPONS & ARMOUR (Equipment) -------------------------------------------------------------------


dagger : ItemDef
dagger =
    { id = "dagger"
    , name = "dagger"
    , glyph = "/"
    , color = "#b9c2d0"
    , kind = Equipment WeaponSlot { damage = 2, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 1
    , maxDepth = 4
    , spawnWeight = 6
    }


shortSword : ItemDef
shortSword =
    { id = "short-sword"
    , name = "short sword"
    , glyph = "/"
    , color = "#d6deea"
    , kind = Equipment WeaponSlot { damage = 4, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 2
    , maxDepth = 6
    , spawnWeight = 5
    }


mace : ItemDef
mace =
    { id = "mace"
    , name = "mace"
    , glyph = "/"
    , color = "#e8b06a"
    , kind = Equipment WeaponSlot { damage = 6, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 4
    }


rapier : ItemDef
rapier =
    { id = "rapier"
    , name = "rapier"
    , glyph = "/"
    , color = "#e0c0d0"
    , kind = Equipment WeaponSlot { damage = 5, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 2
    , maxDepth = 7
    , spawnWeight = 4
    }


vampiricDagger : ItemDef
vampiricDagger =
    { id = "vampiric-dagger"
    , name = "vampiric dagger"
    , glyph = "/"
    , color = "#e0564b"
    , kind = Equipment WeaponSlot { damage = 4, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "vampiric" }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }


blazingMace : ItemDef
blazingMace =
    { id = "blazing-mace"
    , name = "blazing mace"
    , glyph = "/"
    , color = "#ff7a3c"
    , kind = Equipment WeaponSlot { damage = 6, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "blazing" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 2
    }


grimGlaive : ItemDef
grimGlaive =
    { id = "grim-glaive"
    , name = "grim glaive"
    , glyph = "/"
    , color = "#9b6ad8"
    , kind = Equipment WeaponSlot { damage = 7, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "grim" }
    , minDepth = 6
    , maxDepth = 99
    , spawnWeight = 2
    }


thornedMail : ItemDef
thornedMail =
    { id = "thorned-mail"
    , name = "thorned mail"
    , glyph = "["
    , color = "#c08a6a"
    , kind = Equipment ArmourSlot { damage = 0, defense = 4, maxHp = 0, plus = 0, cursed = False, enchant = "thorns" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 2
    }


leatherArmour : ItemDef
leatherArmour =
    { id = "leather-armour"
    , name = "leather armour"
    , glyph = "["
    , color = "#b08968"
    , kind = Equipment ArmourSlot { damage = 0, defense = 1, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 1
    , maxDepth = 4
    , spawnWeight = 6
    }


mailArmour : ItemDef
mailArmour =
    { id = "mail-armour"
    , name = "mail armour"
    , glyph = "["
    , color = "#9aa7ba"
    , kind = Equipment ArmourSlot { damage = 0, defense = 3, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 3
    , maxDepth = 7
    , spawnWeight = 5
    }


plateArmour : ItemDef
plateArmour =
    { id = "plate-armour"
    , name = "plate armour"
    , glyph = "["
    , color = "#cfd8e6"
    , kind = Equipment ArmourSlot { damage = 0, defense = 5, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 5
    , maxDepth = 99
    , spawnWeight = 3
    }



-- RINGS ------------------------------------------------------------------------------------------


ringOfPower : ItemDef
ringOfPower =
    { id = "ring-power"
    , name = "ring of power"
    , glyph = "="
    , color = "#e0a23c"
    , kind = Equipment RingSlot { damage = 2, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


ringOfProtection : ItemDef
ringOfProtection =
    { id = "ring-protection"
    , name = "ring of protection"
    , glyph = "="
    , color = "#6ad8c0"
    , kind = Equipment RingSlot { damage = 0, defense = 2, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


ringOfMight : ItemDef
ringOfMight =
    { id = "ring-might"
    , name = "ring of might"
    , glyph = "="
    , color = "#e0a83c"
    , kind = Equipment RingSlot { damage = 1, defense = 0, maxHp = 8, plus = 0, cursed = False, enchant = "" }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


ringOfForce : ItemDef
ringOfForce =
    { id = "ring-force"
    , name = "ring of force"
    , glyph = "="
    , color = "#e0564b"
    , kind = Equipment RingSlot { damage = 3, defense = 0, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


ringOfEvasion : ItemDef
ringOfEvasion =
    { id = "ring-evasion"
    , name = "ring of evasion"
    , glyph = "="
    , color = "#9be08a"
    , kind = Equipment RingSlot { damage = 0, defense = 3, maxHp = 0, plus = 0, cursed = False, enchant = "" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 3
    }


ringOfTenacity : ItemDef
ringOfTenacity =
    { id = "ring-tenacity"
    , name = "ring of tenacity"
    , glyph = "="
    , color = "#c08a6a"
    , kind = Equipment RingSlot { damage = 1, defense = 1, maxHp = 4, plus = 0, cursed = False, enchant = "" }
    , minDepth = 5
    , maxDepth = 99
    , spawnWeight = 2
    }



-- ARTIFACTS (charge-up relics) -------------------------------------------------------------------


cloakOfShadows : ItemDef
cloakOfShadows =
    { id = "cloak-shadows"
    , name = "cloak of shadows"
    , glyph = "*"
    , color = "#8a7fd6"
    , kind = Artifact { charge = 0, maxCharge = 35, effect = Invisibility 8 }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


hornOfPlenty : ItemDef
hornOfPlenty =
    { id = "horn-plenty"
    , name = "horn of plenty"
    , glyph = "*"
    , color = "#e0c24b"
    , kind = Artifact { charge = 0, maxCharge = 30, effect = Feed 250 }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


chaliceOfBlood : ItemDef
chaliceOfBlood =
    { id = "chalice-blood"
    , name = "chalice of blood"
    , glyph = "*"
    , color = "#e0564b"
    , kind = Artifact { charge = 0, maxCharge = 40, effect = HealHp 12 }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }


timekeepersHourglass : ItemDef
timekeepersHourglass =
    { id = "hourglass"
    , name = "timekeeper's hourglass"
    , glyph = "*"
    , color = "#d6d2c2"
    , kind = Artifact { charge = 0, maxCharge = 45, effect = HasteFor 10 }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }


driedRose : ItemDef
driedRose =
    { id = "dried-rose"
    , name = "dried rose"
    , glyph = "*"
    , color = "#e0564b"
    , kind = Artifact { charge = 0, maxCharge = 50, effect = SummonAlly }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


thievesArmband : ItemDef
thievesArmband =
    { id = "thieves-armband"
    , name = "master thieves' armband"
    , glyph = "*"
    , color = "#c9a0ff"
    , kind = Artifact { charge = 0, maxCharge = 25, effect = Gold 40 }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


etherealChains : ItemDef
etherealChains =
    { id = "ethereal-chains"
    , name = "ethereal chains"
    , glyph = "*"
    , color = "#9aa7ba"
    , kind = Artifact { charge = 0, maxCharge = 20, effect = PullNearest }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }


unstableSpellbook : ItemDef
unstableSpellbook =
    { id = "unstable-spellbook"
    , name = "unstable spellbook"
    , glyph = "*"
    , color = "#82aaff"
    , kind = Artifact { charge = 0, maxCharge = 35, effect = RandomScroll }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }



-- SCROLLS & WANDS --------------------------------------------------------------------------------


scrollOfTeleport : ItemDef
scrollOfTeleport =
    { id = "scroll-teleport"
    , name = "scroll of teleportation"
    , glyph = "?"
    , color = "#c9a0ff"
    , kind = Consumable TeleportSelf
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


scrollOfMagicMapping : ItemDef
scrollOfMagicMapping =
    { id = "scroll-magic-mapping"
    , name = "scroll of magic mapping"
    , glyph = "?"
    , color = "#80cbc4"
    , kind = Consumable MagicMap
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 5
    }


scrollOfIdentify : ItemDef
scrollOfIdentify =
    { id = "scroll-identify"
    , name = "scroll of identify"
    , glyph = "?"
    , color = "#f0c674"
    , kind = Consumable IdentifyAll
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 4
    }


scrollOfUpgrade : ItemDef
scrollOfUpgrade =
    { id = "scroll-upgrade"
    , name = "scroll of upgrade"
    , glyph = "?"
    , color = "#ffd166"
    , kind = Consumable UpgradeGear
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 4
    }


scrollOfRecharging : ItemDef
scrollOfRecharging =
    { id = "scroll-recharging"
    , name = "scroll of recharging"
    , glyph = "?"
    , color = "#82aaff"
    , kind = Consumable Recharge
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 4
    }


scrollOfTerror : ItemDef
scrollOfTerror =
    { id = "scroll-terror"
    , name = "scroll of terror"
    , glyph = "?"
    , color = "#e0564b"
    , kind = Consumable Terror
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 4
    }


scrollOfRemoveCurse : ItemDef
scrollOfRemoveCurse =
    { id = "scroll-remove-curse"
    , name = "scroll of remove curse"
    , glyph = "?"
    , color = "#9be08a"
    , kind = Consumable RemoveCurse
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 4
    }


scrollOfLullaby : ItemDef
scrollOfLullaby =
    { id = "scroll-lullaby"
    , name = "scroll of lullaby"
    , glyph = "?"
    , color = "#a7b0e0"
    , kind = Consumable (Lullaby 6)
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


scrollOfRetribution : ItemDef
scrollOfRetribution =
    { id = "scroll-retribution"
    , name = "scroll of retribution"
    , glyph = "?"
    , color = "#e06ad8"
    , kind = Consumable (Retribution 6)
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


scrollOfTransmutation : ItemDef
scrollOfTransmutation =
    { id = "scroll-transmutation"
    , name = "scroll of transmutation"
    , glyph = "?"
    , color = "#6ad8c0"
    , kind = Consumable Transmute
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 2
    }


scrollOfGrowth : ItemDef
scrollOfGrowth =
    { id = "scroll-growth"
    , name = "scroll of sunlight"
    , glyph = "?"
    , color = "#9be08a"
    , kind = Consumable GrowGrass
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


scrollOfRage : ItemDef
scrollOfRage =
    { id = "scroll-rage"
    , name = "scroll of rage"
    , glyph = "?"
    , color = "#e0824b"
    , kind = Consumable Aggravate
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


scrollOfCorruption : ItemDef
scrollOfCorruption =
    { id = "scroll-corruption"
    , name = "scroll of corruption"
    , glyph = "?"
    , color = "#6fc06a"
    , kind = Consumable Corrupt
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


scrollOfEnchantment : ItemDef
scrollOfEnchantment =
    { id = "scroll-enchantment"
    , name = "scroll of enchantment"
    , glyph = "?"
    , color = "#ffd166"
    , kind = Consumable Enchant
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


scrollOfMysticalEnergy : ItemDef
scrollOfMysticalEnergy =
    { id = "scroll-mystical-energy"
    , name = "scroll of mystical energy"
    , glyph = "?"
    , color = "#82e0ff"
    , kind = Consumable ChargeAbility
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


potionOfCleansing : ItemDef
potionOfCleansing =
    { id = "potion-cleansing"
    , name = "potion of cleansing"
    , glyph = "!"
    , color = "#bfe0d0"
    , kind = Consumable Cleanse
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
    }


wandMagicMissile : ItemDef
wandMagicMissile =
    { id = "wand-magic-missile"
    , name = "wand of magic missile"
    , glyph = "-"
    , color = "#82aaff"
    , kind = Wand { damage = 5, charges = 4, maxCharges = 4, element = "" }
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 4
    }


wandFirebolt : ItemDef
wandFirebolt =
    { id = "wand-firebolt"
    , name = "wand of firebolt"
    , glyph = "-"
    , color = "#ff7a3c"
    , kind = Wand { damage = 9, charges = 3, maxCharges = 3, element = "fire" }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


wandLightning : ItemDef
wandLightning =
    { id = "wand-lightning"
    , name = "wand of lightning"
    , glyph = "-"
    , color = "#9be0ff"
    , kind = Wand { damage = 7, charges = 4, maxCharges = 4, element = "shock" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 3
    }


wandFrost : ItemDef
wandFrost =
    { id = "wand-frost"
    , name = "wand of frost"
    , glyph = "-"
    , color = "#bcd6ff"
    , kind = Wand { damage = 6, charges = 4, maxCharges = 4, element = "frost" }
    , minDepth = 3
    , maxDepth = 99
    , spawnWeight = 3
    }


wandCorrosion : ItemDef
wandCorrosion =
    { id = "wand-corrosion"
    , name = "wand of corrosion"
    , glyph = "-"
    , color = "#9bbf4a"
    , kind = Wand { damage = 5, charges = 3, maxCharges = 3, element = "corrosion" }
    , minDepth = 5
    , maxDepth = 99
    , spawnWeight = 2
    }


wandBlastWave : ItemDef
wandBlastWave =
    { id = "wand-blast-wave"
    , name = "wand of blast wave"
    , glyph = "-"
    , color = "#e0a83c"
    , kind = Wand { damage = 6, charges = 3, maxCharges = 3, element = "blast" }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 2
    }


wandDisintegration : ItemDef
wandDisintegration =
    { id = "wand-disintegration"
    , name = "wand of disintegration"
    , glyph = "-"
    , color = "#c97fe0"
    , kind = Wand { damage = 8, charges = 3, maxCharges = 3, element = "disintegrate" }
    , minDepth = 6
    , maxDepth = 99
    , spawnWeight = 2
    }


wandRegrowth : ItemDef
wandRegrowth =
    { id = "wand-regrowth"
    , name = "wand of regrowth"
    , glyph = "-"
    , color = "#5dd47a"
    , kind = Wand { damage = 2, charges = 4, maxCharges = 4, element = "regrowth" }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 2
    }


{-| The key opens a vault's locked door. Spawn weight 0 keeps it out of the random loot pool — the
generator places one only when it carves a vault — while still being resolvable by id. -}
ironKey : ItemDef
ironKey =
    { id = "key"
    , name = "iron key"
    , glyph = "k"
    , color = "#e8c75a"
    , kind = Key
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    }


{-| The goal: claiming the Amulet on the deepest floor wins the run. The engine places exactly one
(weight 0, so never random) and intercepts its pickup. -}
amulet : ItemDef
amulet =
    { id = "amulet"
    , name = "Amulet of Yendor"
    , glyph = "*"
    , color = "#ffe066"
    , kind = Key
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    }
