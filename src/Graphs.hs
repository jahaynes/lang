module Graphs where

data Node s = 
    Node { item         :: !s
         , reachability :: !Int
         }

    
