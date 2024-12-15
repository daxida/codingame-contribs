import requests
import json
import os

URL = "https://www.codingame.com/services/Contribution/findContribution"

# Requires cookies to display the solution.
COOKIES = {
    "rememberMe": "fillMe",
    "cgSession": "fillMe",
}

CONTRIBUTIONS = {
    "puzzles": [
        ["sorting-requests", "90634bc6b3782045c772c9542dba6e3476542"],
        ["codongame", "778302a4c4149234ae1ee8e5177b79776aa92"],
        ["cg-tags", "76377b03907fe32d1e0b4357b911f7c99717a"],
        ["cg-stub", "759481e6afefea199836a6cb76e15b21d673d"],
        ["function-notation", "67467b18360c683de95549e76097154928a90"],
        ["nadalgo", "652263f52fba00673b2fb8aa2c888e8a27024"],
        ["ruby-parsing", "6251794e1491f5d392a7a064773157491ba4a"],
        ["nickname-filtering", "5832652f1a152c2fe38e96f41c636aeab13e2"],
        ["crazy-eights", "56511956a5b4169dca1558a19400027bbc134"],
        ["euclid-complex", "52518e3c281414497c4f1f13bc85c49c42785"],
        ["coprimes", "50703f8ba46dd4c2eef6629d8d867f14527ed"],
        ["fix-the-spaces", "46053956831b4324bf2a6e36521916e52a727"],
        ["library-dependencies", "439391d1962cacc9a04e76f932b8fc7f43816"],
        ["string-balls", "388573d78acaeca9e34861401284ecfbe5447"],
        ["grid-climbing", "29649c6f594e71b69c8ca2567c8acfa06e207"],
        ["markov-ants", "216638ec81ba9a5dd2105b22ad2de62d3e2df"],
    ],
    "clashes": [
        ["tournament", "76609432329e7c65215f13bf31a32fac86382"],
        ["longest-snake", "745622814caf0e629ed827ff72b44439500cc"],
        ["conjecture", "7040402a6fe461068f5cf5296607c184d043a"],
        ["secretary-problem", "604712d9661c87c342f9ab9013d35c76068ea"],
        ["poll", "59613d02d032fc416f1cbadb39dc79d271194"],
        ["gauss-primes", "554552a272a49fb873235502d28fcc12939c7"],
        ["french", "43379dd82c3d9ac24ac2b9eb3cf8868ddf4f8"],
        ["string-primes", "41661416a26fca8dc6d6de2edf9bc7addc318"],
        ["namesort", "40772d3102acd1dfe76e3dc05b59a34c24014"],
        ["hollerith", "36151a029b6cad971b52d02bfbd8f560e54ad"],
        ["rotate-180", "33642ff63f478245c3e3dfaad3d8692d41dc8"],
        ["perfect-pairs", "32586d50d0adfec06f6269a4358bcb652a789"],
        ["string-reduction", "2856067338ee8581a52298bc586000de4097a"],
        ["traces", "249973095c453ec010fe09b6c2d6c8692fdb9"],
        ["number-box", "242372eba72a892f31afef510eb04f01343af"],
        ["young-diagram", "206742cbca985d49050b0e708d9bf432e8d6e"],
        ["cyclic-groups", "14733a0fd6e257f9020bd21b6c9b25b17327a"],
    ],
}


def fetch(handle: str):
    http = requests.Session()
    res = http.post(
        URL, 
        json=[handle, True], 
        headers={"Content-Type": "application/json"}, 
        cookies=COOKIES
    )
    res.raise_for_status()
    data = json.loads(res.text)["lastVersion"]["data"]
    return data


def main():
    for puzzle_type, entries in CONTRIBUTIONS.items():
        for name, handle in entries:
            contribution_path = f"{puzzle_type}/{name}"
            if not os.path.exists(contribution_path):
                raise FileNotFoundError(contribution_path)

            data = fetch(handle)
            path = f"{puzzle_type}/{name}/json/json"
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                json.dump(data, f, indent=2)


if __name__ == "__main__":
    main()
