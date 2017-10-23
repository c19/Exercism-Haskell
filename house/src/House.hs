module House (rhyme) where

import Data.List (isPrefixOf, intercalate)

bodies = lines
          "that belonged to the farmer sowing his corn\n\
          \that kept the rooster that crowed in the morn\n\
          \that woke the priest all shaven and shorn\n\
          \that married the man all tattered and torn\n\
          \that kissed the maiden all forlorn\n\
          \that milked the cow with the crumpled horn\n\
          \that tossed the dog\n\
          \that worried the cat\n\
          \that killed the rat\n\
          \that ate the malt\n\
          \that lay in the house that Jack built."

heads = filter (isPrefixOf "This") (lines origin)

rhyme :: String
rhyme = (intercalate "\n\n" $ [intercalate "\n" $ (heads !! i):(drop (length bodies - i) bodies) | i <- its]) ++ "\n"
  where its = enumFromThenTo 0 1 (length heads - 1)

origin :: String
origin =
  "This is the house that Jack built.\n\
        \\n\
        \This is the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the horse and the hound and the horn\n\
        \that belonged to the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n"
