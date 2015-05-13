import Control.Monad.Trans.Writer
f = runWriter $ tell "hi" >> return 3
