package com.g2forge.reassert.standard.algorithm.propagate;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.PropagatedUsage;
import com.g2forge.reassert.core.model.contract.usage.GeneralUsage;
import com.g2forge.reassert.standard.algorithm.propagate.StandardUsagePropagation;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

public class TestStandardUsagePropagation {
	@Test
	public void inherits() {
		final GeneralUsage input = GeneralUsage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial).build()).build();
		final Inherits edge = new Inherits();
		final PropagatedUsage expected = new PropagatedUsage(edge, input, Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial, StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy, StandardUsageTerm.UseModified).unspecified(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService, StandardUsageTerm.DistributingBinary, StandardUsageTerm.DistributingSource).build());
		HAssert.assertEquals(expected, StandardUsagePropagation.create().apply(edge, input));
	}
}
