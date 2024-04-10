module DA.WebSockets.Types where

import Data.Aeson.Schema

type NewDonationAlertMsg' =
  [schema|
  {
    result: {
        channel: Text,
        data: {
            seq: Int,
            data: {
                id: Int,
                name: Text,
                username: Text,
                message: Text,
                message_type: Text,
                payin_system: Maybe Text,
                amount: Double,
                currency: Text,
                is_shown: Int,
                amount_in_user_currency: Double,
                recipient_name: Text,
                recipient: {
                    user_id: Int,
                    code: Text,
                    name: Text,
                    avatar: Text
                },
                created_at: Text,
                shown_at: Maybe Text,
                reason: Text
            }
        }
    }
}|]

type NewDonationAlertMsg = Object NewDonationAlertMsg'