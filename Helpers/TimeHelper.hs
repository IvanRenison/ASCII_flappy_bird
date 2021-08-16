module Helpers.TimeHelper (
    nominalDiffTimeToMicroSeconds
) where

import Data.Time.Clock ( nominalDiffTimeToSeconds, NominalDiffTime )


nominalDiffTimeToMicroSeconds :: NominalDiffTime -> Int 
nominalDiffTimeToMicroSeconds time = round $ nominalDiffTimeToSeconds time * 1000000


