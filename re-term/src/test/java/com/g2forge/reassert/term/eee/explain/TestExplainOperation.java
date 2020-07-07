package com.g2forge.reassert.term.eee.explain;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.term.eee.explain.model.IdentityExplainedOperation;
import com.g2forge.reassert.term.eee.explain.model.ZeroExplainedOperation;
import com.g2forge.reassert.term.eee.express.Literal;

public class TestExplainOperation extends ATestExplanationRenderer {
	@Test
	public void identity() {
		HAssert.assertEquals("1 (*) - because the * operator (with identity 1) was applied", render(ExplanationMode.Explain, new IdentityExplainedOperation<Integer>(1, "*", 1)));
		HAssert.assertEquals("1 (*) - because there's a single argument: 1", render(ExplanationMode.Explain, new IdentityExplainedOperation<Integer>(1, "*", 1).toBuilder().argument$(true, new Literal<>(1)).build()));
		HAssert.assertEquals("2 (*) - because the * operator (with identity 1) was applied\n\t* 2 - Non-identity", render(ExplanationMode.Explain, new IdentityExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
		HAssert.assertEquals("2 (*) - because the * operator (with identity 1) was applied\n\t_ 1\n\t* 2 - Non-identity", render(ExplanationMode.Trace, new IdentityExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
		HAssert.assertEquals("2 (*) - because there's a single relevant argument: 1", render(ExplanationMode.Summarize, new IdentityExplainedOperation<Integer>(2, "*", 1).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("Non-identity", 2)).build()));
	}

	@Test
	public void zero() {
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0)));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t* 0", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(true, new Literal<>(0)).build()));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t* 0 - ZERO!", render(ExplanationMode.Explain, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
		HAssert.assertEquals("0 (*) - because 0 is the zero of the * operator\n\t_ 1\n\t* 0 - ZERO!", render(ExplanationMode.Trace, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
		HAssert.assertEquals("0 (*) - because one or more arguments were 0, for example: 0 - ZERO!", render(ExplanationMode.Summarize, new ZeroExplainedOperation<Integer>("*", 0).toBuilder().argument$(false, new Literal<>(1)).argument$(true, new Literal<>("ZERO!", 0)).build()));
	}
}
