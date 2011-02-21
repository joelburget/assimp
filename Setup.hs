import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemStdout)
import Distribution.Verbosity
import System.Directory (setCurrentDirectory)

main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> makeLib a b >> preBuild simpleUserHooks a b }

makeLib :: Args -> BuildFlags -> IO ()
makeLib _ flags = do
    setCurrentDirectory "./assimp"
    rawSystemStdout (fromFlag $ buildVerbosity flags) "env"
        ["cmake", "-DENABLE_BOOST_WORKAROUND=ON"]
    rawSystemExit (fromFlag $ buildVerbosity flags) "env"
        ["CFLAGS=-D_LIB", "make"]
