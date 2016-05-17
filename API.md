## GET /api/0/:repo/tags

#### Authentication



Clients must supply the following data


#### Captures:

- *repo*: The name of the repository

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"total_tags":2,"tags":["tag1","tag2"]}
```

## GET /api/0/error_codes

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"ENOCOMMENT":"Comment not found","ENOREQ":"Pull-Request not found","ENOISSUE":"Issue not found","ENOTASSIGNED":"This request must be assigned to be merged","EDBERROR":"An error occured at the database level and prevent the action from reaching completion","ENOTASSIGNEE":"Only the assignee can merge this review","ENOCODE":"Variable message describing the issue","EINVALIDTOK":"Invalid or expired token. Please visit https://pagure.io/ to get or renew your API token.","EPRSCORE":"This request does not have the minimum review score necessary to be merged","EINVALIDREQ":"Invalid or incomplete input submited","ENOPROJECTS":"No projects found","EPULLREQUESTSDISABLED":"Pull-Request have been deactivated for this project","ENOUSER":"No such user found","ETRACKERDISABLED":"Issue tracker disabled for this project","ENOPRCLOSE":"You are not allowed to merge/close pull-request for this project","ENOPROJECT":"Project not found","EISSUENOTALLOWED":"You are not allowed to view this issue"}
```

## GET /api/0/fork/:username/:repo/tags

#### Authentication



Clients must supply the following data


#### Captures:

- *username*: The username of the user
- *repo*: The name of the repository

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"total_tags":2,"tags":["tag1","tag2"]}
```

## GET /api/0/groups

#### Authentication



Clients must supply the following data


#### GET Parameters:

- pattern
     - **Values**: *Fed*, *web*, *ora, ...*
     - **Description**: An optional pattern to filter by


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{"groups":["Fedora-Infra"],"total_groups":1}
```

- 

```javascript
{"groups":["Fedora-Infra","fedora-web"],"total_groups":2}
```

## GET /api/0/user/:username

#### Authentication



Clients must supply the following data


#### Captures:

- *username*: The username of the user

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"repos":[{"parent":null,"settings":{"Minimum_score_to_merge_pull-request":-1,"Web-hooks":null,"pull_requests":true,"Only_assignee_can_merge_pull-request":false,"project_documentation":false,"issue_tracker":true},"user":{"fullname":"Ricky Elrod","name":"codeblock"},"name":"testrepo","id":4,"date_created":"1426595173","description":"test description"}],"user":{"fullname":"Ricky Elrod","name":"codeblock"},"forks":[]}
```

## GET /api/0/users

#### Authentication



Clients must supply the following data


#### GET Parameters:

- pattern
     - **Values**: *Fed*, *web*, *ora, ...*
     - **Description**: An optional pattern to filter by


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{"users":["codeblock"],"total_users":1}
```

- 

```javascript
{"users":["codelock","janedoe"],"total_users":2}
```

## GET /api/0/version

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"version":"0.6"}
```


