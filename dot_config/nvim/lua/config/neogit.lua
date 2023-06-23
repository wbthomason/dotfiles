require('neogit').setup {
  disable_commit_confirmation = true,
  disable_signs = true,
  disable_builtin_notifications = false,
  integrations = { diffview = true },
  sections = {
    untracked = {
      folded = false
    },
    unstaged = {
      folded = false
    },
    staged = {
      folded = false
    },
    stashes = {
      folded = true
    },
    unpulled = {
      folded = false
    },
    unmerged = {
      folded = false
    },
    recent = {
      folded = false
    },
  },
}
