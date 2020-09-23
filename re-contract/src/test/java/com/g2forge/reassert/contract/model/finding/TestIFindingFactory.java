package com.g2forge.reassert.contract.model.finding;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.finding.rule.ConditionFinding;
import com.g2forge.reassert.contract.model.finding.rule.IRuleFinding;

public class TestIFindingFactory {
	@Test
	public void test() {
		final IFindingFactory<IRuleFinding> factory = ConditionFinding::new;
		HAssert.assertEquals(ConditionFinding.class, factory.getFindingType().getErasedType());
	}
}
