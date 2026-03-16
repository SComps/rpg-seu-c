def create_binary_data(filename):
    records = [
        "John Doe".ljust(20) + "123 Maple St, Springfield".ljust(30) + "555-0101".ljust(15) + "john.doe@example.com".ljust(35),
        "Jane Smith".ljust(20) + "456 Oak Ave, Metropolis".ljust(30) + "555-0202".ljust(15) + "jane.smith@test.org".ljust(35),
        "Bob Johnson".ljust(20) + "789 Pine Rd, Gotham City".ljust(30) + "555-0303".ljust(15) + "bob.j@workmail.com".ljust(35),
        "Alice Williams".ljust(20) + "321 Cedar Ln, Smallville".ljust(30) + "555-0404".ljust(15) + "alice.w@provider.net".ljust(35)
    ]
    with open(filename, 'wb') as f:
        for r in records:
            f.write(r.encode('ascii'))

create_binary_data(r'c:\Users\Scott\.gemini\antigravity\scratch\rpg-seu-c\Examples\custdata_std.bin')
