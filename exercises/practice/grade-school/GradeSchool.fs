module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let grade (number: int) (school: School) : string list =
    school |> Map.tryFind number |> Option.defaultValue []

let private exists student school =
    school
    |> Map.values
    |> Seq.collect id
    |> Seq.exists (fun name -> name = student)

let add (student: string) (key: int) (school: School) : School =
    if school |> exists student then
        school
    else
        let students = grade key school
        school |> Map.add key (student :: students |> List.sort)

let roster (school: School) : string list =
    school |> Map.toList |> List.collect snd |> List.distinct
