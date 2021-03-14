module Global.Unsafe where

import Foreign (Foreign)

foreign import unsafeStringify :: Foreign -> String
