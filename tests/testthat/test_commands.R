test_that("BASIC commands work", {
  expect_equal(basic('10 LET I = 2\n 20 LET I = I + 1\n 999 END')$run()$vars$I, 3)
  expect_equal(basic('5 READ A\n 20 DATA -7\n 999 END')$run()$vars$A, -7)
  expect_equal(basic('5 LET B = 2*(1.5+SIN(2))\n 999 END')$run()$vars$B, 2*(1.5+sin(2)))

  expect_equal(
    capture.output(
      basic('10 IF 20<=30 THEN 50\n 20 PRINT 1\n 50 PRINT 2\n 100 END\n')$run()
    ), "2"
  )

  expect_equal(basic(
  '5 LET A = 0\n
  10 FOR I = 1 TO 20\n
  15 FOR J = 1 TO 25\n
  20 LET A = A + I*J\n
  30 NEXT J\n
  40 NEXT I\n
  999 END')$run()$vars$A, sum(1:20 %*% t(1:25)))

  expect_equal(basic('5 DIM D(50,15), B(20) \n
                     10 LET A = B(10) \n999 END \n')$run()$vars$A, 0)

  expect_equal(basic('5 DEF F(X) = X^X\n
                     10 LET A = F(3)^2\n 999 END \n')$run()$vars$A, (3^3)^2)
})


