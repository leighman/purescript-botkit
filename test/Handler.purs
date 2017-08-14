module Test.Handler where

import Prelude

import Botkit.Slack.Handler (HandlerM, allUsers, getUser, saveUser, runHandlerM)
import Botkit.Slack.Types (BOTKIT, RawBot, RawController, RawMessage, RawUser, UserId(..))
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

newtype User = User RawUser

unUser :: User -> RawUser
unUser (User u) = u

instance showUser :: Show User where
  show = userShow

instance eqUser :: Eq User where
  eq = userEq

foreign import userShow :: User -> String
foreign import userEq :: User -> User -> Boolean
foreign import makeMockUser :: UserId -> User

foreign import data RawStorage :: Type
foreign import goodStorage :: RawStorage
foreign import badStorage :: RawStorage
foreign import makeMockController :: RawStorage -> RawController
foreign import mockBot :: RawBot
foreign import mockMessage :: RawMessage

run :: forall e a. RawController -> HandlerM e a -> Aff e a
run = \c h -> runHandlerM h c mockBot mockMessage

handlerSpec :: forall e. Spec (botkit :: BOTKIT | e) Unit
handlerSpec = do

  describe "Handler" do
    let uid = UserId "foo"

    describe "Good cases" do
      let c = makeMockController goodStorage
      it "getUser works" do
        user <- run c $ getUser uid
        (User <$> user) `shouldEqual` Just (makeMockUser uid)
      it "saveUser works" do
        let u = unUser $ makeMockUser uid
        run c $ saveUser u
      it "allUsers works" do
        us <- run c $ allUsers
        (User <$> us) `shouldEqual` []

    describe "Error cases" do
      let c = makeMockController badStorage
      it "getUser shouldError" do
        expectError $ run c $ getUser uid
      it "saveUser shouldError" do
        let u = unUser $ makeMockUser uid
        expectError $ run c $ saveUser u
      it "allUsers shouldError" do
        expectError $ run c $ allUsers
