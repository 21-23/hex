import qualified System.Process.Typed as TypedProcess
import           ServiceIdentity (ServiceType(StateService))
import           System.Environment (setEnv )

import           Test.Tasty
import           Test.Tasty.Hspec

import qualified WebSocketControl
import qualified HexProcess
import qualified Docker

main :: IO ()
main = do
  -- disable concurrency because we're running against single Docker instance
  -- would be great to be able to make only integration test sequential,
  -- but currently it's not possible https://github.com/feuerbach/tasty#compile-time
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain =<< testSpec "integration tests" specs

specs :: Spec
specs = do
  before_ (Docker.stopContainersByImage
      [ messenger_service
      , test_service
      , state_service]) $ do

    describe "running containers" $ do
      it "starts messenger service and init sequence" $ do
        images <- HexProcess.whileRunning Docker.getPsImages
        images `shouldContain` [messenger_service]
        images `shouldContain` [test_service]

      it "stops containers before quitting" $ do
        HexProcess.runAndStop
        images <- Docker.getPsImages
        images `shouldNotContain` [messenger_service]
        images `shouldNotContain` [test_service]

      it "builds image if not built" $ do
        Docker.removeImage test_service
        runningImages <- HexProcess.whileRunning Docker.getPsImages
        builtImages   <- Docker.getBuiltImages
        runningImages `shouldContain` [test_service]
        builtImages   `shouldContain` [test_service]

    describe "controlling via websockets" $ do
      it "starts requested container" $ do
        images <- HexProcess.withProcess $ \p -> do
          WebSocketControl.requestService "localhost" 3002 StateService
          HexProcess.waitUntilServiceStarted p StateService
          Docker.getPsImages
        images `shouldContain` [state_service]

      it "quits when shutdown is requested" $ do
        HexProcess.withProcess $ \p -> do
          WebSocketControl.requestShutdown "localhost" 3002
          TypedProcess.waitExitCode p
        images <- Docker.getPsImages
        images `shouldNotContain` [messenger_service]
        images `shouldNotContain` [test_service]
  where
    messenger_service = "hex_messenger"
    test_service = "hex_test-service"
    state_service = "hex_state-service"
