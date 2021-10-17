import requests
import argparse


api_urls = {
    "ethgas": "https://ethgasstation.info/api/ethgasAPI.json",
    "ifconfig": "http://ifconfig.co"
}


def print_eth_gas():
    api_url = api_urls['ethgas']

    eth_gas_json = requests.get(api_url).json()

    safe_low = eth_gas_json['safeLow'] / 10
    avg_price = eth_gas_json['average'] / 10
    fast_price = eth_gas_json['fast'] / 10
    fastest_price = eth_gas_json['fastest'] / 10

    print(f"Safe Low Price in GEWI: {safe_low}")
    print(f"Average Price in GEWI: {avg_price}")
    print(f"Fast Price in GEWI: {fast_price}")
    print(f"Fastest Price in GEWI: {fastest_price}")


def print_ifconfig():
    api_url = api_urls['ifconfig']

    headers = {
        "User-Agent": "curl/7.55.1"
    }
    print(requests.get(api_url, headers=headers).text)


api_funcs = {
    "ethgas": print_eth_gas,
    "ifconfig": print_ifconfig
}


def main():
    parser = argparse.ArgumentParser(description="Get API name to fetch")
    parser.add_argument("api_name", metavar="<API NAME>", type=str)
    args = parser.parse_args()

    if args.api_name in str(api_funcs.keys()):
        api_funcs[args.api_name]()


if __name__ == "__main__":
    main()
