# Games whose running score doesn't end at the final score were parsed with
# bugs fixed in nflfastR 6.0.0 (nflfastR#625): every Jacksonville home game of
# 2001 and 2002 credited the home team's touchdowns and safeties to the
# visitors, and 2011_13_DET_NO had two touchdowns twice, so their
# score_differential (and the vegas_wp computed from it) is wrong. Until the
# released data are rebuilt, the model scripts rebuild those games with
# nflfastR, as nflfastR's own WP model does. Needs nflfastR 6.0.0+.
#
# `pbp` needs game_id, home_score, away_score, total_home_score and
# total_away_score. Rebuilt games keep the columns of `pbp`.
repair_broken_games <- function(pbp) {
  running_score_ok <- function(p) {
    p |>
      dplyr::group_by(game_id) |>
      dplyr::summarise(
        ok = max(total_home_score, na.rm = TRUE) == dplyr::first(home_score) &
          max(total_away_score, na.rm = TRUE) == dplyr::first(away_score),
        .groups = "drop"
      )
  }
  ok <- running_score_ok(pbp)
  bad <- ok$game_id[!ok$ok]
  cat("games rebuilt because the running score doesn't end at the final:\n")
  print(bad)
  if (length(bad) == 0) {
    return(pbp)
  }
  rebuilt <- nflfastR::build_nflfastR_pbp(bad, decode = FALSE) |>
    dplyr::select(dplyr::all_of(names(pbp)))
  if (!all(running_score_ok(rebuilt)$ok)) {
    stop(
      "Rebuilt games still have the wrong running score: needs nflfastR 6.0.0+"
    )
  }
  dplyr::bind_rows(dplyr::filter(pbp, !game_id %in% bad), rebuilt)
}
