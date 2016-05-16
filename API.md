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


