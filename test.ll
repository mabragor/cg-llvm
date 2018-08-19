define fastcc i64 @"fun-get-char"() {
    %res.0 = call i32 @getchar()
    %res.1 = sext i32 %res.0 to i64
    ret i64 %res.1
}