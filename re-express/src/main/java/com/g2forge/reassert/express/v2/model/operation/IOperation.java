package com.g2forge.reassert.express.v2.model.operation;

import java.util.List;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.express.v2.model.HExpression;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;

public interface IOperation<Name, Value> extends IExpression<Name, Value> {
	public interface IOperationBuilder<Name, Value, Builder extends IOperationBuilder<Name, Value, Builder, Built>, Built extends IOperation<Name, Value>> extends IBuilder<Built> {
		public Builder argument(IExpression<Name, Value> argument);

		public default Builder argument$(Name name, Value value) {
			return argument(new Literal<>(name, value));
		}

		public default Builder argument$(Value value) {
			return argument(new Literal<>(value));
		}

		public default Built valid() {
			final Built retVal = build();
			if (!retVal.getOperator().isValid(retVal.getArguments())) throw new IllegalArgumentException();
			return retVal;
		}
	}

	public interface IOperator {
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder();

		@Note(type = NoteType.TODO, value = "Validation should include explanations")
		public default boolean isValid(List<? extends IExpression<?, ?>> arguments) {
			return true;
		}
	}

	public List<? extends IExpression<Name, Value>> getArguments();

	public IOperator getOperator();

	@Override
	public default boolean isSame(IExpression<?, ?> that) {
		if (this == that) return true;
		if ((that == null) || !(that instanceof IOperation)) return false;

		final IOperation<?, ?> cast = (IOperation<?, ?>) that;
		if (!getOperator().equals(cast.getOperator())) return false;
		if (!HExpression.isSame(getArguments(), cast.getArguments())) return false;
		return true;
	}
}
