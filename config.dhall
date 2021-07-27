let Post
    : Type
    = { title : Text, date : Text, path : Text }

let mkPost
    : Text → Text → Text → Post
    = λ(title : Text) → λ(date : Text) → λ(path : Text) → { title, date, path }

in  [ mkPost "Hello" "2021-07-25" "./posts/hello.md"
    , mkPost "类型论初步" "2021-02-05" "./posts/类型论初步.md"
    ]
