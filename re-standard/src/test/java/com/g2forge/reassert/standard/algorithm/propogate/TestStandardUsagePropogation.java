package com.g2forge.reassert.standard.algorithm.propogate;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.Usage;
import com.g2forge.reassert.standard.algorithm.propogate.StandardUsagePropogation;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

public class TestStandardUsagePropogation {
	@Test
	public void inherits() {
		final Usage input = Usage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial).build()).build();
		final Usage expected = Usage.builder().name("Inherits() Usage").terms(Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial, StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy, StandardUsageTerm.UseModified).unspecified(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService, StandardUsageTerm.DistributingBinary, StandardUsageTerm.DistributingSource).build()).build();
		HAssert.assertEquals(expected, StandardUsagePropogation.create().apply(new Inherits(), input));
	}
}
