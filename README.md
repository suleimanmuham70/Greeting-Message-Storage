# 👋 Greeting Message Storage

A simple **Clarity smart contract** for storing and managing greeting messages on the Stacks blockchain. Perfect for learning basic storage operations and event handling! ✨

## 🚀 Features

- 💬 **Set Personal Greetings**: Store your personalized greeting message
- ✏️ **Update Messages**: Modify your existing greeting anytime  
- 🗑️ **Delete Greetings**: Remove your greeting from storage
- 📊 **Message History**: Track all messages with timestamps
- 🔍 **Search & Filter**: Find messages by prefix or get recent messages
- 📈 **Analytics**: View total message counts and user statistics
- 🛡️ **Admin Controls**: Contract owner can manage settings and bulk operations
- 🎯 **Events**: Emits events for all major operations

## 📋 Contract Functions

### Public Functions

#### 📝 Message Management
- `(set-greeting (message (string-utf8 280)))` - Set your greeting message
- `(update-greeting (new-message (string-utf8 280)))` - Update existing greeting
- `(delete-greeting)` - Delete your greeting message

#### 👑 Admin Functions  
- `(set-max-message-length (new-length uint))` - Set maximum message length
- `(toggle-contract-status)` - Enable/disable contract functionality
- `(bulk-set-greetings (messages (list 10 {user: principal, message: (string-utf8 280)})))` - Bulk set messages

#### 🔍 Query Functions
- `(get-recent-messages (limit uint))` - Get recent messages with limit
- `(get-multiple-greetings (users (list 20 principal)))` - Get multiple user greetings

### Read-Only Functions

#### 📖 Data Retrieval
- `(get-greeting (user principal))` - Get user's current greeting
- `(get-user-message-count (user principal))` - Get user's total message count
- `(get-message-by-id (message-id uint))` - Get specific message by ID
- `(get-total-messages)` - Get total number of messages
- `(get-contract-info)` - Get contract status and configuration

#### ⚙️ Settings
- `(get-max-message-length)` - Get current max message length
- `(is-active)` - Check if contract is active

## 🛠️ Usage Examples

### Basic Usage

```clarity
;; Set a greeting message
(contract-call? .greeting-message-storage set-greeting u"Hello, Stacks community! 🎉")

;; Get someone's greeting
(contract-call? .greeting-message-storage get-greeting 'SP1ABCD...)

;; Update your greeting
(contract-call? .greeting-message-storage update-greeting u"Updated: Hello World! 🌍")

;; Delete your greeting
(contract-call? .greeting-message-storage delete-greeting)
```

### Advanced Usage

```clarity
;; Get contract information
(contract-call? .greeting-message-storage get-contract-info)

;; Get recent 10 messages
(contract-call? .greeting-message-storage get-recent-messages u10)

;; Get multiple user greetings at once
(contract-call? .greeting-message-storage get-multiple-greetings 
  (list 'SP1ABCD... 'SP2EFGH... 'SP3IJKL...))

;; Search messages by prefix
(contract-call? .greeting-message-storage search-messages-by-prefix u"Hello")
```

### Admin Functions

```clarity
;; Set maximum message length (owner only)
(contract-call? .greeting-message-storage set-max-message-length u200)

;; Toggle contract active status (owner only)  
(contract-call? .greeting-message-storage toggle-contract-status)

;; Bulk set greetings (owner only)
(contract-call? .greeting-message-storage bulk-set-greetings 
  (list 
    {user: 'SP1ABCD..., message: u"Welcome!"}
    {user: 'SP2EFGH..., message: u"Hello there!"}
  ))
```

## 📊 Data Structures

### User Greeting
```clarity
{
  message: (string-utf8 280),    ;; The greeting message
  timestamp: uint,               ;; Block height when set
  message-count: uint           ;; User's total message count
}
```

### Message History
```clarity
{
  sender: principal,            ;; Message sender
  message: (string-utf8 280),   ;; The message content
  timestamp: uint,              ;; Block height
  message-id: uint             ;; Unique message ID
}
```

## 🎯 Events

The contract emits the following events:

- `greeting-set` - When a new greeting is stored
- `greeting-updated` - When a greeting is modified  
- `greeting-deleted` - When a greeting is removed
- `max-length-updated` - When max message length changes
- `contract-status-changed` - When contract is enabled/disabled
- `bulk-greetings-set` - When bulk operation is performed

## ⚠️ Error Codes

- `u100` - Owner only operation
- `u101` - Invalid message (empty)
- `u102` - Message too long
- `u103` - Message not found
- `u104` - Unauthorized (contract inactive)

## 🏗️ Development

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity language

### Setup
```bash
# Clone the repository
git clone <repository-url>
cd greeting-message-storage

# Check contract syntax
clarinet check

# Run tests
clarinet test

# Deploy locally
clarinet console
```

### Testing
```bash
# Run all tests
clarinet test

# Run specific test file
clarinet test tests/greeting-message-storage_test.ts
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is open source and available under the [MIT License](LICENSE).

## 🎓 Learning Objectives

This contract demonstrates:
- ✅ **Basic Storage**: Using maps and data variables
- ✅ **Event Emission**: Printing structured events
- ✅ **Error Handling**: Proper assertion and error codes
- ✅ **Access Control**: Owner-only functions
- ✅ **Data Validation**: Input validation and constraints
- ✅ **Batch Operations**: Processing multiple items
- ✅ **Query Functions**: Different ways to retrieve data

Perfect for developers learning Clarity fundamentals! 🎉

---

**Made with ❤️ for the Stacks ecosystem**
