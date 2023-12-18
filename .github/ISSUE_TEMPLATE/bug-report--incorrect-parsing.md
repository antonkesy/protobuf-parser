---
name: 'Bug report: Incorrect parsing'
about: Report a incorrect or not working protobuf parser
title: "[BUG]: "
labels: bug
assignees: ''

---

Thank you for reporting an issue! **Please fill-out following sections**:

**Describe the bug**
A clear and concise description of what the bug is.

**Protobuf File/Text**
Please replace the content of the code block with the actual value.
```
import "foo.proto";

message SearchRequest {
  int32 page_number = 2;
  double results_per_page = 3;
}
```

**Expected behavior**
Please replace the content of the code block with the actual value.
```
Protobuf {syntax = Nothing, package = Nothing, imports = ["foo.proto"], options = [], enums = [], messages = [Message "SearchRequest" [ImplicitMessageField (Scalar (IntType Int32)) "page_number" 2 [],ImplicitMessageField (Scalar (FloatType D
ouble)) "results_per_page" 3 []]], services = []}
```
