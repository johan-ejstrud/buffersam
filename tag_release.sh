# Add tag with version number to latest commit in main branch
if [ `git branch --show-current` != 'main' ]; then
  echo ERROR: Must be on main branch.
  exit 1;
fi

git tag "v`grep Version DESCRIPTION | cut -d' ' -f2`"
git push --tags
