import com.google.inject.AbstractModule

import services.{ TransactionService, InMemoryTransactionService }

class Module extends AbstractModule {
  override def configure() = {
    bind(classOf[TransactionService]).to(classOf[InMemoryTransactionService])
  }
}
