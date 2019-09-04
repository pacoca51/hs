type Host = String
type Username = String
type Repository = String
type Git = (Username,Host,Repository)

git :: Git
git = ("pacoca51","github.com","hs")

account (u,h,_) = u++"@"++h
https (u,h,r) = h++"/"++u++"/"++r
repo (u,_,r) = u++"/"++r