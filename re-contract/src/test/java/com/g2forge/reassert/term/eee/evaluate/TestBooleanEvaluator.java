package com.g2forge.reassert.term.eee.evaluate;

import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.term.eee.evaluate.IEvaluator;
import com.g2forge.reassert.term.eee.evaluate.bool.BooleanBooleanSystem;
import com.g2forge.reassert.term.eee.evaluate.bool.BooleanEvaluator;
import com.g2forge.reassert.term.eee.express.IConstant;
import com.g2forge.reassert.term.eee.express.Literal;
import com.g2forge.reassert.term.eee.express.Operation;

import lombok.Getter;

public class TestBooleanEvaluator {
	@Getter(lazy = true)
	private static final IEvaluator<Boolean, Boolean> evaluator = new BooleanEvaluator(BooleanBooleanSystem.create());

	protected static void reduction(Operation.Operator operator, BinaryOperator<Boolean> accumulator) {
		for (int n = 1; n < 4; n++) {
			for (int i = 0; i < (1 << n); i++) {
				final Operation.OperationBuilder<Boolean> builder = operator.<Boolean>builder();
				boolean expected = (i & 0x1) != 0;
				for (int j = 0; j < n; j++) {
					final boolean value = ((i >>> j) & 0x1) != 0;
					builder.argument$(value);
					expected = accumulator.apply(expected, value);
				}
				final Operation<Boolean> actual = builder.build();
				HAssert.assertEquals(String.format("Failed with %1$d arguments to %2$s: %3$s", n, operator, actual.getArguments().stream().map(e -> (IConstant<Boolean>) e).map(IConstant::get).map(Object::toString).collect(Collectors.joining(", "))), expected, getEvaluator().eval(actual));
			}
		}
	}

	@Test
	public void and() {
		reduction(Operation.Operator.AND, Boolean::logicalAnd);
	}

	@Test
	public void literal() {
		HAssert.assertEquals(false, getEvaluator().eval(new Literal<>(false)));
		HAssert.assertEquals(false, getEvaluator().eval(new Literal<>(false)));
	}
	
	@Test
	public void not() {
		HAssert.assertEquals(true, getEvaluator().eval(Operation.Operator.NOT.<Boolean>builder().argument$(false).build()));
		HAssert.assertEquals(false, getEvaluator().eval(Operation.Operator.NOT.<Boolean>builder().argument$(true).build()));
	}

	@Test
	public void or() {
		reduction(Operation.Operator.OR, Boolean::logicalOr);
	}
}
