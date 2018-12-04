case "abc" in
    123) echo "err #1" ;;
    abc) echo "ok #1"  ;;
esac

case "567" in
    123 | 567) echo "ok #2" ;;
    *) echo "err #2"  ;;
esac

case "xxABCyy" in
    xxABCyyzz) echo "err #3" ;;
    xx*yy) echo "ok #3" ;;
    *) echo "err #4"  ;;
esac

case "aaa" in
    123) echo "err #5" ;;
    *) echo "ok #4"  ;;
esac

case "a*d"e in
    123) echo "err #6" ;;
    "a*d*") echo "bad #7"  ;;
    "a*d"*) echo "ok #5"  ;;
    *) echo "err #8"  ;;
esac
