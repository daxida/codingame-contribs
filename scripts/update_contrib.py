"""Update the testcases of a contribution."""

import requests
import json
import datetime

CONTRIB_HANDLE = ""

CODINGAME_MAX_NUMBER_TESTCASES = 60  # That is, 30 testpairs


cookies = {
    "rememberMe": "fillMe",
    "cgSession": "fillMe",
}

headers = {
    "accept": "application/json, text/plain, */*",
    "content-type": "application/json;charset=UTF-8",
}

json_data = [
    CONTRIB_HANDLE,
    True,
]


def json_print(json_file):
    print(json.dumps(json_file, indent=2))


def get_contribution_testcases(contribution):
    return contribution["lastVersion"]["data"]["testCases"]


def fetch_contribution():
    response = requests.post(
        "https://www.codingame.com/services/Contribution/findContribution",
        cookies=cookies,
        headers=headers,
        json=json_data,
    )

    if response.status_code == 200:
        print("Fetched contribution successfully.")
        return response.json()
    else:
        print(f"Failed to fetch contribution: {response.status_code}")
        return None


def load_backup(backup_filename):
    try:
        with open(backup_filename, "r") as backup_file:
            contribution = json.load(backup_file)
            # Pretty print the contribution for debugging
            json_print(contribution)
        return contribution
    except FileNotFoundError:
        print(f"Backup file {backup_filename} not found.")
        return None
    except json.JSONDecodeError:
        print("Error decoding JSON from the backup file.")
        return None


def backup_contribution(contribution):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    backup_filename = f"contribution_backup_{timestamp}.json"

    with open(backup_filename, "w") as backup_file:
        json.dump(contribution, backup_file, indent=4)

    print(f"Contribution backed up to {backup_filename}")
    return backup_filename


def modify_contribution(contribution, new_data, overwrite):
    # new_data should be given in a json-like array of dicts
    #
    # [
    #     {
    #         "title": "Test 1",
    #         "isTest": true,
    #         "testIn": "123",
    #         "testOut": "123",
    #         "isValidator": false,
    #         "needValidation": true
    #     },
    #     {
    #         "title": "Validator 1",
    #         "isTest": false,
    #         "testIn": "123",
    #         "testOut": "123",
    #         "isValidator": true,
    #         "needValidation": true
    #     },
    #     ...
    # ]
    testcases = get_contribution_testcases(contribution)

    assert len(testcases) % 2 == 0
    assert len(new_data) % 2 == 0

    # Simply add the tests at the end
    # ... but what if we wanted to replace the current tests?
    if not overwrite:
        assert (
            len(new_data) + len(testcases) <= CODINGAME_MAX_NUMBER_TESTCASES
        ), f"There are too many ({len(new_data) + len(testcases)}) testcases."

        testcases.extend(new_data)
    else:
        assert len(new_data) <= CODINGAME_MAX_NUMBER_TESTCASES

        # If there are 14 testcases and we want to add 20,
        # only the LAST 4 will be replaced
        testcases = testcases[: CODINGAME_MAX_NUMBER_TESTCASES - len(new_data)]
        testcases.extend(new_data)

        # The slice 3 lines above is weird.
        contribution["lastVersion"]["data"]["testCases"] = testcases

    print("Contribution has been modified.")


def post_updated_contribution(updated_contribution):
    update_url = "https://www.codingame.com/services/Contribution/updateContribution"
    response = requests.post(
        update_url,
        cookies=cookies,
        headers=headers,
        json=[
            updated_contribution["codingamerId"],
            CONTRIB_HANDLE,
            updated_contribution["type"],
            updated_contribution["lastVersion"]["data"],
            True,
            False,
            updated_contribution["activeVersion"],
        ],
    )

    if response.status_code == 200:
        print("Contribution updated successfully.")
    else:
        print(f"Failed to update contribution: {response.status_code}")
        print(response.text)
        json_print(updated_contribution)


def gen_testpairs(number_testpairs_to_add):
    # This should be implemented / copied from somewhere else
    try:
        from generator import gen
    except ImportError:
        print(f"Could not find the generator file.")
        exit(1)

    new_data = []

    for idx in range(1, number_testpairs_to_add + 1):
        public = gen()
        validator = gen()

        template = [
            {
                "title": f"{idx} public",
                "isTest": True,
                "testIn": public,
                "testOut": "todo",
                "isValidator": False,
                "needValidation": True,
            },
            {
                "title": f"{idx} validator",
                "isTest": False,
                "testIn": validator,
                "testOut": "todo",
                "isValidator": True,
                "needValidation": True,
            },
        ]

        new_data.extend(template)

    return new_data


def main():
    contribution = fetch_contribution()
    json_print(contribution)

    # file = "contribution_backup_2024-08-17_00-58-02.json"
    # contribution = load_backup(file)

    if not contribution:
        print("This should never happen.")
        return

    backup_contribution(contribution)

    number_testpairs_to_add = 40
    number_testpairs_to_add = min(
        number_testpairs_to_add,
        (CODINGAME_MAX_NUMBER_TESTCASES - len(get_contribution_testcases(contribution)))
        // 2,
    )
    number_testpairs_to_add = 30

    new_data = gen_testpairs(number_testpairs_to_add)
    assert len(new_data) == 2 * number_testpairs_to_add

    modify_contribution(contribution, new_data, overwrite=True)

    # json_print(contribution)
    testcases = get_contribution_testcases(contribution)
    json_print(testcases)
    print(f"{len(testcases)=}")

    post_updated_contribution(contribution)


if __name__ == "__main__":
    main()
