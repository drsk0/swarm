class GQLApi {
  // queries
  static String queryAllBallots = """
  query QueryAllBallots {
    qAllBallots
  }
  """;

  static String subscribeAllBallots = """
  subscription SubscribeAllBallots {
    sAllBallots
  }
  """;

  static String queryAllOptions = """
    query QueryAllOptions(\$ballotId: String!) {
      qAllOptions(qaoBId: \$ballotId)
    }
  """;

  static String subscribeAllOptions = """
    subscription SubscribeAllOptions(\$ballotId: String!) {
      sAllOptions(qaoBId: \$ballotId)
    }
  """;

  // TODO (drsk) remove and only use queryOption
  static String queryAllArguments = """
    query QueryAllArguments(\$ballotId: String!, \$oId: String!, \$proContra: IProContra!) {
      qAllArguments(qaaBId: \$ballotId, qaaOId: \$oId, qaaProContra: \$proContra)
    }
  """;

  static String subscribeAllArguments = """
    subscription QueryAllArguments(\$ballotId: String!, \$oId: String!, \$proContra: IProContra!) {
      sAllArguments(qaaBId: \$ballotId, qaaOId: \$oId, qaaProContra: \$proContra)
    }
  """;

  static String queryArgumentById = """
    query QueryArgumentById(\$ballotId: String!, \$optionId: String!, \$argId: String!) {
      qArgumentById(qabBId: \$ballotId, qabOId: \$optionId, qabAId: \$argId) {
        aUserId
        aText
        aProContra
        aUpVotes {
          uvUserId
        }
      }
  }
  """;

  static String subscribeArgumentById = """
    subscription SubscribeArgumentById(\$ballotId: String!, \$optionId: String!, \$argId: String!) {
      sArgumentById(qabBId: \$ballotId, qabOId: \$optionId, qabAId: \$argId) {
        aUserId
        aText
        aProContra
        aUpVotes {
          uvUserId
        }
      }
  }
  """;

  static String queryBallotResult = """
    query queryBallotResult(\$ballotId: String!) {
      qBallotResult(qbrBId: \$ballotId) {
        brVotes {
            key
            value
        }
        brTurnout
        brOptions {
          wiId
          wiValue {
            oTitle
            oDescription
            oUserId
            oVotes {
                key
                value {
                  vStars
                }
            }
          }
        }
      }
    }
  """;

  static String subscribeBallotResult = """
    subscription SubscribeBallotResult(\$ballotId: String!) {
      sBallotResult(qbrBId: \$ballotId) {
        brVotes {
            key
            value
        }
        brTurnout
        brOptions {
          wiId
          wiValue {
            oTitle
            oDescription
            oUserId
            oVotes {
                key
                value {
                  vStars
                }
            }
          }
        }
      }
    }
  """;

  static String queryBallotById = """
  query QueryBallotById(\$ballotId: String!) {
    qBallotById(qbiBId: \$ballotId) {
      bTitle
      bDescription
      bCreator
      bDelegations {
        dDelegatee
      }
    }
  }
  """;

  static String subscribeBallotById = """
  subscription QueryBallotById(\$ballotId: String!) {
    sBallotById(qbiBId: \$ballotId) {
      bTitle
      bDescription
      bCreator
      bDelegations {
        dDelegatee
      }
    }
  }
  """;

  static String queryOption = """
  query QueryOption(\$ballotId: String! \$oId: String!) {
    qOption(qobBId: \$ballotId, qobOId : \$oId) {
        oTitle
        oDescription
        oUserId
        oVotes {
            key
            value {
              vStars
            }
        }
        oArguments {
          key
          value {
            aProContra
          }
        }
  }
  }
  """;

  static String subscribeOption = """
  subscription SubscribeOption(\$ballotId: String! \$oId: String!) {
    sOption(qobBId: \$ballotId, qobOId : \$oId) {
        oTitle
        oDescription
        oUserId
        oVotes {
            key
            value {
              vStars
            }
        }
        oArguments {
          key
          value {
            aProContra
          }
        }
    }
  }
  """;

  static String queryUserSwarms = """
  query QueryUserSwarms {
    qUserSwarms {
      wrRef
      wrValue
    }
  }
  """;

  static String queryFish = """
  query QueryFish {
    qFish {
      fName
      fUserId
      fProfile
    }
  }
  """;

  static String queryFishByUserId = """
  query QueryProfile (\$qfUserId: String!){
    qFishByUserId (qfUserId: \$qfUserId){
      fName
      fUserId
      fProfile
      }
  }
  """;

  // mutations
  static String updateFcmToken = """
  mutation UpdateFcmToken (\$uftToken : String!) {
    mUpdateFcmToken (
      uftToken : \$uftToken
    )
  }
  """;

  static String createBallot = """
  mutation CreateBallot(\$cbaTitle: String!, \$cbaDescription: String!) {
    mCreateBallot(cbaTitle: \$cbaTitle, cbaDescription: \$cbaDescription)
  }
  """;

  static String deleteBallot = """
  mutation DeleteBallot(\$dbaBallotId: String!) {
    mDeleteBallot(dbaBallotId: \$dbaBallotId)
  }
  """;

  static String createVote = """
  mutation CreateVote (\$cvaBallotId: String!, \$cvaOptionId: String!, \$cvaStars: NrOfStars!) {
    mCreateVote(cvaBallotId: \$cvaBallotId, cvaOptionId: \$cvaOptionId, cvaStars: \$cvaStars)
  }
  """;

  static String createDelegation = """
  mutation CreateDelegation (\$dmaBallotId: String!, \$dmaDelegateeId: String!) {
    mCreateDelegation(dmaBallotId: \$dmaBallotId, dmaDelegateeId: \$dmaDelegateeId)
  }
  """;

  static String createArgument = """
    mutation CreateArgument(\$caaBallotId: String!, \$caaUserId: String!, \$caaOptionId: String!,
        \$caaText: String!, \$caaProContra: IProContra!) {
      mCreateArgument (caaBallotId: \$caaBallotId, caaUserId: \$caaUserId, caaOptionId: \$caaOptionId, caaText: \$caaText, caaProContra: \$caaProContra)
    }
  """;

  static String createUpVote = """
    mutation CreateUpVote(\$cuaBallotId: String!, \$cuaOptionId: String!, \$cuaArgumentId: String!) {
      mCreateUpVote (cuaBallotId: \$cuaBallotId, cuaOptionId: \$cuaOptionId, cuaArgumentId:
          \$cuaArgumentId)
    }
  """;

  static String deleteUpVote = """
    mutation CreateUpVote(\$cuaBallotId: String!, \$cuaOptionId: String!, \$cuaArgumentId: String!) {
      mDeleteUpVote (duaBallotId: \$cuaBallotId, duaOptionId: \$cuaOptionId, duaArgumentId:
          \$cuaArgumentId)
    }
  """;

  static String createOption = """
    mutation CreateOption(\$coaBallotId: String!, \$coaTitle: String!, \$coaDescription: String!) {
      mCreateOption (coaTitle: \$coaTitle, coaDescription: \$coaDescription, coaBallotId: \$coaBallotId)
    }
  """;

  static String deleteOption = """
    mutation DeleteOption(\$doaBallotId: String!, \$doaOptionId: String!) {
      mDeleteOption (doaBallotId: \$doaBallotId, doaOptionId: \$doaOptionId)
    }
  """;

  static String createSwarm = """
      mutation CreateSwarm (\$csaName: String!) {
        mCreateSwarm (csaName: \$csaName)
      }
  """;

  static String createSwarm0 = """
      mutation CreateSwarm (\$csaName: String!) {
        mCreateSwarm0 (csaName: \$csaName)
      }
  """;

  static String createInvite = """
      mutation CreateInvite (\$ciaToken: String!) {
        mCreateInvite (ciaToken: \$ciaToken)
      }
  """;

  static String takeInvite = """
      mutation TakeInvite (\$tiaToken: String!) {
        mTakeInvite (tiaToken: \$tiaToken)
      }
  """;

  static String updateProfile = """
      mutation UpdateProfile (\$upaBody: String!) {
        mUpdateProfile(upaBody: \$upaBody)
      }
  """;
}
