{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn)
import HebarLib

tocli Pkg{..} = intercalate "\t" [pkgname, pkgversion, pkgcabal, pkgtar]

main = do
  names <- fmap pack <$> getArgs
  mapM_ (putStrLn . tocli) $ catMaybes (topkg <$> names)
  
