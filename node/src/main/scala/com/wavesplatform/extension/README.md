# Commitment Extension

This extension automatically creates and broadcasts `CommitToGenerationTransaction` at the beginning of each generation period.

## Logic

The extension monitors the blockchain height and when a new generation period starts, it creates a commitment transaction for the next period.

### Balance Checks

- If the account balance is less than the transaction fee, an error is logged.
- If the account balance is less than 100 WAVES, a warning is logged.

### Logging

When a transaction is successfully created, the following information is logged:
- Transaction ID
- BLS Public Key
- Commitment Signature

## Configuration

To enable this extension, add its fully qualified class name to the `waves.extensions` list in your node's configuration file.

Example:
```
waves {
  # ... other settings

  extensions = [
    # ... other extensions
    com.wavesplatform.extension.CommitmentExtension
  ]
}
```
