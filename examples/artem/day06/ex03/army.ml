class ['a] army =
  object
    val mutable _members : 'a list = []
    method get_members = _members
    method add (member : 'a) = _members <- member :: _members

    method delete =
      match _members with [] -> () | _ :: rest -> _members <- rest

    method size = List.length _members
  end
