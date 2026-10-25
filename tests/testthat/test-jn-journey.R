ep_small <- function() {
  data.frame(
    patient_id = c("P1", "P1", "P1", "P1", "P2", "P2", "P3"),
    state = c("A", "A", "B", "A", "B", "B", "A"),
    start = as.Date(c("2022-01-01", "2022-02-05", "2022-03-20", "2022-06-01",
                      "2022-01-10", "2022-05-01", "2022-02-01")),
    end = as.Date(c("2022-01-30", "2022-03-06", "2022-04-18", "2022-06-30",
                    "2022-02-08", "2022-05-30", "2022-02-28")),
    stringsAsFactors = FALSE
  )
}

test_that("jn_simulate gives claims-like rows with an unused level", {
  ep <- jn_simulate(50)
  expect_named(ep, c("patient_id", "code", "state", "start", "end"))
  expect_equal(length(unique(ep$patient_id)), 50)
  expect_s3_class(ep$start, "Date")
  expect_true(all(ep$end >= ep$start))
  expect_true(all(grepl("^6299[0-9]{5}$", ep$code)))
  expect_equal(levels(ep$state), paste("Drug", LETTERS[1:5]))
  expect_equal(sum(ep$state == "Drug E"), 0)
  expect_identical(jn_simulate(30, seed = 2), jn_simulate(30, seed = 2))
  em <- jn_simulate(30, unit = "month")
  expect_true(all(format(em$start, "%d") == "01"))
  expect_error(jn_simulate(0))
})

test_that("jn_states keeps order and fixes colours", {
  st <- jn_states(c("A", "B", "(end)"))
  expect_equal(st$level, c("A", "B", "(end)"))
  expect_equal(st$colour[3], "grey75")
  st2 <- jn_states(c("A", "B"), colours = c(B = "red"))
  expect_equal(st2$colour[2], "red")
  expect_error(jn_states(c("A", "A")))
})

test_that("jn_sequence merges, collapses, cuts and appends the end state", {
  j <- jn_sequence(ep_small(), states = c("A", "B", "C"))
  expect_s3_class(j, "journey")
  s <- j$sequence
  p1 <- s[s$patient_id == "P1", ]
  # A (Jan-Mar, gap 4 days merged), B, A (gap 44 days but a different state), end
  expect_equal(as.character(p1$state), c("A", "B", "A", "(end)"))
  expect_equal(p1$end[1], as.Date("2022-03-06"))
  # P2: B, long gap, B -> collapsed into one step
  p2 <- s[s$patient_id == "P2", ]
  expect_equal(as.character(p2$state), c("B", "(end)"))
  expect_equal(levels(s$state), c("A", "B", "C", "(end)"))
  expect_equal(j$meta$n_persons, 3)

  j2 <- jn_sequence(ep_small(), collapse = FALSE, end_state = NULL)
  expect_equal(as.character(j2$sequence$state[j2$sequence$patient_id == "P2"]), c("B", "B"))

  j3 <- jn_sequence(ep_small(), gap_state = "(gap)")
  expect_equal(as.character(j3$sequence$state[j3$sequence$patient_id == "P2"]),
               c("B", "(gap)", "B", "(end)"))

  j4 <- jn_sequence(ep_small(), max_steps = 2)
  expect_true(max(j4$sequence$step) <= 2)
  expect_equal(as.character(j4$sequence$state[j4$sequence$patient_id == "P1"]), c("A", "B"))
})

test_that("jn_sequence works on month data and validates input", {
  em <- jn_simulate(40, unit = "month")
  j <- jn_sequence(em, unit = "month")
  expect_equal(j$meta$grace, 1)
  expect_true(all(format(j$sequence$start, "%d") == "01"))
  expect_error(jn_sequence(ep_small(), states = c("A")), "not in")
  expect_error(jn_sequence(ep_small(), id = "nope"), "not found")
  expect_output(print(j), "journey")
})

test_that("journey() validates a hand-made sequence", {
  sq <- data.frame(patient_id = c("1", "1", "2"), step = c(1, 2, 1),
                   state = c("A", "B", "A"), start = Sys.Date(), end = Sys.Date())
  j <- journey(sq, states = c("A", "B", "C"))
  expect_equal(levels(j$sequence$state), c("A", "B", "C"))
  sq$step[2] <- 3
  expect_error(journey(sq), "step")
})

test_that("jn_flows lists every pair, zeros included", {
  j <- jn_sequence(ep_small(), states = c("A", "B", "C"))
  fl <- jn_flows(j)
  expect_equal(nrow(fl), 3 * 4 * 4)
  ab <- fl[fl$step_from == 1 & fl$from == "A" & fl$to == "B", ]
  expect_equal(ab$n, 1)
  expect_equal(sum(fl$n[fl$to == "C"]), 0)
  expect_equal(nrow(jn_flows(j, steps = 1)), 0)
})

test_that("jn_tree positions children inside their parent", {
  j <- jn_sequence(jn_simulate(200), max_steps = 3)
  tr <- jn_tree(j)
  expect_equal(sum(tr$n[tr$depth == 1]), 200)
  kids <- tr[tr$depth == 2, ]
  par <- tr[match(kids$parent, tr$id), ]
  expect_true(all(kids$start >= par$start - 1e-12 & kids$end <= par$end + 1e-12))
  expect_equal(max(jn_tree(j, depth = 2)$depth), 2)
})
