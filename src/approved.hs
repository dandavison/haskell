{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Network.HTTP.Types (status400)

import GHC.Generics

import Data.Aeson.Types

import qualified Web.Scotty as Scotty

import qualified Github.Data as Github
import qualified Github.Private
import qualified Github.Issues.Comments as IssueComments


main = do

  [user, pass] <- fmap words (readFile "auth")
  let
    port = 12893
    auth = Github.Private.GithubBasicAuth (BS.pack user) (BS.pack pass)
    reviewers = ["Reviewer_2", "Reviewer_2"]
    signOffComments = ["approved", "lgtm"]

  Scotty.scotty port $ do

    Scotty.get "/:owner/:repo/pull/:pullID" $ do

      owner <- Scotty.param "owner"
      repo <- Scotty.param "repo"
      pullID <- Scotty.param "pullID"

      liftIO . putStrLn $ "/" ++ owner ++ "/" ++ repo ++ "/pull/" ++ (show pullID)

      possibleComments <- liftIO $ IssueComments.comments' (Just auth) owner repo pullID
      case possibleComments of
        (Left error) -> errorResponse error
        (Right comments) -> approvalResponse reviewers signOffComments comments

    Scotty.get "/" $ do
      Scotty.text "Pull Request Approval API"


-- A comment is an 'approval comment' if it was made by a person with
-- the appropriate authority and its content is one of the phrases in
-- signOffComments.
isApproval :: [String] -> [String] -> IssueComments.IssueComment -> Bool
isApproval reviewers signOffComments comment =
  (elem user reviewers) && (elem commentBody signOffComments)
  where
    user = Github.githubOwnerLogin . Github.issueCommentUser $ comment
    commentBody = canonicalize $ Github.issueCommentBody comment


-- Strip ignored characters from the back of the string and lowercase
-- the result.
canonicalize :: String -> String
canonicalize comment =
  map toLower (stripTrailing comment)
  where
    stripTrailing = reverse . dropWhile isIgnoredChar . reverse
    isIgnoredChar c = elem c " \t\r\n!."


data ApprovalResponse = ApprovalResponse {
  approved :: Bool,
  approverLogin :: Maybe String
} deriving Generic

data ErrorResponse = ErrorResponse {
  error :: String
} deriving Generic

instance ToJSON ApprovalResponse
instance ToJSON ErrorResponse


-- Respond with JSON data indicating whether any of the comments is an
-- 'approval comment'.
approvalResponse reviewers signOffComments comments = do
  Scotty.json $ ApprovalResponse approved approverLogin
  liftIO . putStrLn $ (show . length $ approvalComments) ++ " approval comments out of " ++ (show . length $ comments)
  where
    approvalComments = filter (isApproval reviewers signOffComments) comments
    approved = (> 0) . length $ approvalComments
    approvingComment = if approved then (Just $ last approvalComments) else Nothing
    approverLogin = fmap (Github.githubOwnerLogin . Github.issueCommentUser) approvingComment


-- TODO: Differentiate. This returns 400 for a variety of errors
-- occurring during the github client's attempt to fetch comments.
errorResponse error = do
  Scotty.status status400
  Scotty.json $ ErrorResponse (show error)
  liftIO . putStrLn $ "Error: " ++ (show error)
