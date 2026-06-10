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
        ]
    , bosses =
        [ gnollWarlord
        , spiderQueen
        ]
    , items =
        [ healingPotion
        , greaterHealingPotion
        , potionOfStrength
        , potionOfShielding
        , potionOfRegeneration
        , potionOfHaste
        , ration
        , torch
        , goldPile
        , dagger
        , shortSword
        , mace
        , leatherArmour
        , mailArmour
        , plateArmour
        , scrollOfTeleport
        , scrollOfMagicMapping
        , scrollOfIdentify
        , scrollOfUpgrade
        , wandMagicMissile
        , wandFirebolt
        , wandLightning
        , ringOfPower
        , ringOfProtection
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
    , minDepth = 1
    , maxDepth = 3
    , spawnWeight = 10
    , xp = 1
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
    , minDepth = 2
    , maxDepth = 4
    , spawnWeight = 6
    , xp = 2
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
    , minDepth = 2
    , maxDepth = 5
    , spawnWeight = 8
    , xp = 3
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
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 5
    , xp = 4
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
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 6
    , xp = 4
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
    , minDepth = 4
    , maxDepth = 8
    , spawnWeight = 5
    , xp = 6
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
    , minDepth = 3
    , maxDepth = 7
    , spawnWeight = 4
    , xp = 2
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
    , minDepth = 2
    , maxDepth = 6
    , spawnWeight = 4
    , xp = 3
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
    , minDepth = 2
    , maxDepth = 7
    , spawnWeight = 3
    , xp = 4
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
    , minDepth = 4
    , maxDepth = 8
    , spawnWeight = 4
    , xp = 7
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
    , minDepth = 4
    , maxDepth = 4
    , spawnWeight = 0
    , xp = 30
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
    , minDepth = 7
    , maxDepth = 7
    , spawnWeight = 0
    , xp = 60
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
    , spawnWeight = 10
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
    , spawnWeight = 4
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
    , kind = Equipment WeaponSlot { damage = 2, defense = 0, maxHp = 0, plus = 0 }
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
    , kind = Equipment WeaponSlot { damage = 4, defense = 0, maxHp = 0, plus = 0 }
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
    , kind = Equipment WeaponSlot { damage = 6, defense = 0, maxHp = 0, plus = 0 }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 4
    }


leatherArmour : ItemDef
leatherArmour =
    { id = "leather-armour"
    , name = "leather armour"
    , glyph = "["
    , color = "#b08968"
    , kind = Equipment ArmourSlot { damage = 0, defense = 1, maxHp = 0, plus = 0 }
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
    , kind = Equipment ArmourSlot { damage = 0, defense = 3, maxHp = 0, plus = 0 }
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
    , kind = Equipment ArmourSlot { damage = 0, defense = 5, maxHp = 0, plus = 0 }
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
    , kind = Equipment RingSlot { damage = 2, defense = 0, maxHp = 0, plus = 0 }
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
    , kind = Equipment RingSlot { damage = 0, defense = 2, maxHp = 0, plus = 0 }
    , minDepth = 2
    , maxDepth = 99
    , spawnWeight = 3
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


wandMagicMissile : ItemDef
wandMagicMissile =
    { id = "wand-magic-missile"
    , name = "wand of magic missile"
    , glyph = "-"
    , color = "#82aaff"
    , kind = Wand { damage = 5, charges = 4, maxCharges = 4, burns = False }
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
    , kind = Wand { damage = 9, charges = 3, maxCharges = 3, burns = True }
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
    , kind = Wand { damage = 7, charges = 4, maxCharges = 4, burns = False }
    , minDepth = 4
    , maxDepth = 99
    , spawnWeight = 3
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
