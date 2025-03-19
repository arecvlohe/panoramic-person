{-
   Consumers of this API are only concerned with with how to:
   create a Person, edit a Person, and transform a Person.
   If a Person is properly validated, they are returned Just (Person Validated)
   which they can access via pattern matching.

   They are not exposed the internals of this API, such as how the person
   is validated, to reduce API surface and churn. By hiding those internals,
   they can change and update without impacting how the rest of the API is used.

   In that sense, consumers are not concerned about how a Person is validated,
   only that they have what they need to properly validate a Person.

-}


module Main exposing
    ( MaritalStatus
    , createPerson
    , editFirstName
    , editLastName
    , editNumber
    , editPerson
    , editSocial
    , editStatus
    , validatePerson
    )


type Editing
    = Editing


type Validated
    = Validated



{-
   Marital status is best shaped as a union.
   The narrowing of the type enforces proper semantics
-}


type MaritalStatus
    = Single
    | Married
    | JointFiling
    | SeparateFiling
    | HeadOfHousehold
    | SurvivingSpouse


type alias Fields =
    { firstName : String
    , lastName : String
    , socialSecurityNumber : String
    , maritalStatus : MaritalStatus
    , phoneNumber : String
    }



{-
   Creating a phantom type so that consumers are required
   to create a Person Editing, and edit them, before being able
   to transform that person into a Person Validated. Ensures
   proper usage of Person Editing and Person Validated
-}


type Person state
    = Person Fields


createPerson : Person Editing
createPerson =
    Person
        { firstName = ""
        , lastName = ""
        , socialSecurityNumber = ""
        , maritalStatus = Single
        , phoneNumber = ""
        }



{-
   Ability to edit a person all at once as well
   as specific fields. If I had made this a Dict
   instead of a Record, there would have been more
   ergonomics around editing/updating a person but
   would lose out on statically defined keys
-}


editPerson : Person Editing -> Person Editing
editPerson (Person fields) =
    Person fields



{-
   These helper functions are defined in such a way so that
   they can be piped from one to another, which is a nice ergonomic
   benefit of working in Elm
-}


editFirstName : String -> Person Editing -> Person Editing
editFirstName name (Person fields) =
    Person { fields | firstName = name }


editLastName : String -> Person Editing -> Person Editing
editLastName name (Person fields) =
    Person { fields | lastName = name }


editSocial : String -> Person Editing -> Person Editing
editSocial social (Person fields) =
    Person { fields | socialSecurityNumber = social }


editStatus : MaritalStatus -> Person Editing -> Person Editing
editStatus status (Person fields) =
    Person { fields | maritalStatus = status }


editNumber : String -> Person Editing -> Person Editing
editNumber number (Person fields) =
    Person { fields | phoneNumber = number }


validatePerson : Person Editing -> Maybe (Person Validated)
validatePerson (Person fields) =
    if
        validateFirstName fields.firstName
            && validateLastName fields.lastName
            && validateSocial fields.socialSecurityNumber
            -- marital status need not be validated because its semantics are enforced in a union
            && validateNumber fields.phoneNumber
    then
        Just (Person fields)

    else
        Nothing



{-
   The validations are rather simple but more complex checking
   would be helpful to ensure their validity such as phone number
   having a valid area code.

   I am only implementing these small function here so that it compiles
-}


validateFirstName : String -> Bool
validateFirstName =
    not << String.isEmpty


validateLastName : String -> Bool
validateLastName =
    not << String.isEmpty



{-
   I considered making SSN an Int as opposed to a String
   but that seemed to make it more difficult to validate because
   there aren't helpers to validate the length of an Int. I think it's better
   to make it a String, consider the length, and then parse it into an Int
   to validate that it only contains digits

   If I was really wanting to make clear distictions with these numbers
   I'd create newtypes for ssn and phone number to make that more clear
   in the code
-}


validateSocial : String -> Bool
validateSocial ssn =
    (ssn |> String.isEmpty |> not)
        && (String.length ssn == 10)
        && (ssn
                |> String.toInt
                |> (\m ->
                        case m of
                            Just _ ->
                                True

                            Nothing ->
                                False
                   )
           )



{-
   Phone numbers, plus area code, are numbers that are 10 digits long
   and only contains digits
-}


validateNumber : String -> Bool
validateNumber number =
    (number |> String.isEmpty |> not)
        && (String.length number == 10)
        && (number
                |> String.toInt
                |> (\m ->
                        case m of
                            Just _ ->
                                True

                            Nothing ->
                                False
                   )
           )
