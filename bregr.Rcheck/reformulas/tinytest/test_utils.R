expect_equal(reOnly(~ 1 + x + y + (1|f) + (1|g)), ~(us(1 | f)) + (us(1 | g)))
expect_equal(addForm0(y~x,~1), y ~ x+1)
expect_equal(addForm0(~x,~y), ~x+y)
ff <- findbars_x(y~1+(x|f/g))

## deparse in tests to avoid having to deal with raw expressions/language objects
efun <- function(x, y, FUN=expandAllGrpVar) expect_equal(deparse1(FUN(x)), y)
efun(ff, "list(us(x | g:f), us(x | f))")
efun(quote(1|(f/g)/h), "list(1 | h:g:f, 1 | g:f, 1 | f)")
efun(quote(1|f/g/h),  "list(1 | h:g:f, 1 | g:f, 1 | f)")
efun(quote(1|f*g), "list(1 | f, 1 | g, 1 | f:g)")
efun(quote(1|f+g), "list(1 | f, 1 | g)")
efun(quote(a+b|f+g+h*i), "list(a + b | f, a + b | g, a + b | h, a + b | i, a + b | h:i)")
efun(quote(s(log(d), k = 4)), "list(s(log(d), k = 4))")
efun(quote(s(log(d+1))), "list(s(log(d + 1)))")
efun(quote(us(x,n=2)),
     "list(fixedFormula = ~1, reTrmFormulas = list(x), reTrmAddArgs = list(us(n = 2)), reTrmClasses = \"us\")",
     FUN = splitForm)
efun(quote((1 | a / (b*c))), "list((1 | c:b:a), (1 | c:a), (1 | b:a), (1 | a))")
efun(quote((1 | a / (b+c))), "list((1 | c:a), (1 | b:a), (1 | a))")


efun <- function(x, y, ..., FUN=findbars_x) expect_equal(deparse1(FUN(x, ...)), y)
efun(~ 1 + (x + y || g), expand_doublevert_method = "diag_special", y = "list(diag(x + y | g))")
efun(~ 1 + (x + y || g), expand_doublevert_method = "split",
     y = "list(us(1 | g), us(0 + x | g), us(0 + y | g))")
efun(~ 1 + (1 | f) + (1 | g), y = "list(us(1 | f), us(1 | g))")
efun(~ 1 + (1|h) + (x + y || g), expand_doublevert_method = "split",
     y = "list(us(1 | h), us(1 | g), us(0 + x | g), us(0 + y | g))")
efun(~ 1 + (1|h) + (x + y || g), expand_doublevert_method = "split", default.special = NULL,
     y = "list(1 | h, 1 | g, 0 + x | g, 0 + y | g)")
efun(~ 1 + (1|Subject), "list(us(1 | Subject))")
efun(~ (1||Subject), "list(diag(1 | Subject))")
efun(~ (1|Subject), "list(us(1 | Subject))")
efun(~ (1|Subject), default.special = NULL, y = "list(1 | Subject)")
expect_equal(findbars_x(~ 1 + x), NULL)
expect_equal(findbars_x(~ s(x, bs = "tp")), NULL)
efun(y ~ a + log(b) + s(x, bs = "tp") + s(y, bs = "gp"), target = "s", default.special = NULL,
     y = "list(s(x, bs = \"tp\"), s(y, bs = \"gp\"))")     
expect_true(inForm(z~.,quote(.)))
expect_false(inForm(z~y,quote(.)))
expect_true(inForm(z~a+b+c,quote(c)))
expect_false(inForm(z~a+b+(d+e),quote(c)))
f <- ~ a + offset(x)
f2 <- z ~ a
expect_true(inForm(f,quote(offset)))
expect_false(inForm(f2,quote(offset)))

efun3 <- function(x, y, ..., FUN=extractForm) expect_equal(deparse1(FUN(x, ...)), y)
efun3(~a+offset(b),quote(offset), y="list(offset(b))")
expect_equal(extractForm(~c,quote(offset)), NULL)
efun3(~a+offset(b)+offset(c),quote(offset), y="list(offset(b), offset(c))")
efun3(~offset(x), quote(offset), y="list(offset(x))")

expect_equal(dropHead(~a+offset(b),quote(offset)), quote(a+b))
expect_equal(dropHead(~a+poly(x+z,3)+offset(b),quote(offset)), quote(a+poly(x+z,3)+b))
expect_equal(drop.special(x~a + b+ offset(z)), quote(x~a+b))
expect_equal(replaceForm(quote(a(b+x*c(y,z))),quote(y),quote(R)), quote(a(b+x*c(R,z))))

ss <- ~(1 | cask:batch) + (1 | batch)
expect_equal(replaceForm(ss,quote(cask:batch),quote(batch:cask)), ~(1 | batch:cask) + (1 | batch))
expect_equal(replaceForm(ss, quote(`:`), quote(`%:%`)), ~(1 | cask %:% batch) + (1 | batch))

