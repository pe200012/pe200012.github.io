let Post
    : Type
    = { title : Text, date : Text, path : Text }

let mkPost
    : Text → Text → Text → Post
    = λ(title : Text) → λ(date : Text) → λ(path : Text) → { title, date, path }

in  [ mkPost "Hello" "2021-07-25" "./posts/hello.md" ]
